module Lox (main) where

import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Par4 (Par,parse,lit,sat,alts,opt,some,many,noError,skip,context,position,reject,Pos(..))
import System.Environment(getArgs)
import System.Exit(ExitCode(..),exitWith)
import System.IO (stderr,hFlush,hPutStrLn)
import Text.Printf (printf)
import qualified Data.Char as Char

main :: IO ()
main = do
  getArgs >>= \case
    [] -> error "repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- readFile filename
      case parse "Expect expression" gram contents of
        Right prog -> do
          execute env0 prog
        Left err ->
          parseError err
      pure ()

parseError :: String -> IO ()
parseError = abort 65

runtimeError :: Pos -> String -> IO a
runtimeError Pos{line} mes = abort 70 (printf "%s\n[line %d]" mes line)

abort :: Int -> String -> IO a
abort code mes = do
  putErr mes
  exitWith (ExitFailure code)

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr
  pure ()

----------------------------------------------------------------------
-- Ast

data Prog
  = Prog [Decl]

data Decl
  = DStat Stat
  | DVarDecl Identifier (Maybe Exp)

data Stat
  = SExp Exp
  | SPrint Exp
  | SBlock [Decl]

data Exp
  = ELit Lit
  | EGrouping Exp
  | EBinary Pos Exp Op2 Exp
  | EUnary Pos Op1 Exp
  | EVar Identifier
  | EAssign Identifier Exp

data Lit
  = LNil
  | LBool Bool
  | LNumber Double
  | LString String

data Op1 = Negate | Not

data Op2 = Add | Sub | Mul | Div | Equals | NotEquals | Less | LessEqual | Greater | GreaterEqual

data Identifier = IdentifierX { pos :: Pos, name :: String }

----------------------------------------------------------------------
-- Environment

data Env = Env (Map String (IORef Value))

env0 :: Env
env0 = Env Map.empty

insertEnv :: Env -> Identifier -> Value -> IO Env
insertEnv (Env m) IdentifierX{name} v = do
  r <- newIORef v
  pure $ Env (Map.insert name r m)

lookupEnv :: Env -> Identifier -> IO (IORef Value)
lookupEnv (Env m) IdentifierX{name,pos} =
  case Map.lookup name m of
    Just r -> pure r
    Nothing -> do
      runtimeError pos (printf "Undefined variable '%s'." name)

readEnv :: Env -> Identifier -> IO Value
readEnv env x = do
  r <- lookupEnv env x
  readIORef r

assignEnv :: Env -> Identifier -> Value -> IO ()
assignEnv env x v = do
  r <- lookupEnv env x
  writeIORef r v

----------------------------------------------------------------------
-- Execution

execute :: Env -> Prog -> IO ()
execute env = \case
  Prog decs ->
    execDs env decs

execDs :: Env -> [Decl] -> IO ()
execDs env = \case
  [] -> pure ()
  d1:ds -> do
    execD env d1 $ \env ->
      execDs env ds

execD :: Env -> Decl -> (Env -> IO ()) -> IO ()
execD env = \case
  DStat stat -> \k -> do
    execS env stat
    k env
  DVarDecl id eopt -> \k -> do
    v <-
      case eopt of
        Just e -> evaluate env e
        Nothing -> pure VNil
    env' <- insertEnv env id v
    k env'

execS :: Env -> Stat -> IO ()
execS env = \case
  SExp e -> do
    _ <- evaluate env e
    pure ()
  SPrint e -> do
    v <- evaluate env e
    print v
  SBlock decls -> do
    execDs env decls

----------------------------------------------------------------------
-- Evaluation

evaluate :: Env -> Exp -> IO Value
evaluate env = eval

  where
    eval = \case
      ELit x -> pure (evalLit x)
      EGrouping e -> eval e
      EBinary pos e1 op e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        evalOp2 pos v1 v2 op
      EUnary pos op e -> do
        v <- eval e
        evalOp1 pos op v
      EVar x -> do
        readEnv env x
      EAssign x e -> do
        v <- eval e
        assignEnv env x v
        pure v

    evalLit = \case
      LNil{} -> VNil
      LBool b -> VBool b
      LNumber n -> VNumber n
      LString s -> VString s

    evalOp1 pos = \case
      Negate -> vnegate pos
      Not -> \v -> pure (VBool (not (isTruthy v)))

    evalOp2 pos v1 v2 = \case
      Add -> vadd pos (v1,v2)
      Sub -> VNumber <$> binary pos (-) v1 v2
      Mul -> VNumber <$> binary pos (*) v1 v2
      Div -> VNumber <$> binary pos (/) v1 v2
      Equals -> pure (VBool (vequal v1 v2))
      NotEquals -> pure (VBool (not (vequal v1 v2)))
      Less -> VBool <$> binary pos (<) v1 v2
      LessEqual -> VBool <$> binary pos (<=) v1 v2
      Greater -> VBool <$> binary pos (>) v1 v2
      GreaterEqual -> VBool <$> binary pos (>=) v1 v2

    binary pos f v1 v2 = do
      n1 <- getNumber pos "Operands must be numbers." v1
      n2 <- getNumber pos "Operands must be numbers." v2
      pure (f n1 n2)

vnegate :: Pos -> Value -> IO Value
vnegate pos v1 = do
  n1 <- getNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True

getNumber :: Pos -> String -> Value -> IO Double
getNumber pos message = \case
  VNumber n -> pure n
  _ -> runtimeError pos message

vadd :: Pos -> (Value,Value) -> IO Value
vadd pos = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> runtimeError pos "Operands must be two numbers or two strings."

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  _ -> False

----------------------------------------------------------------------
-- Values

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s

----------------------------------------------------------------------
-- Parse

gram :: Par Prog
gram = program where

  keywords =
    [ "false"
    , "nil"
    , "print"
    , "this"
    , "true"
    , "var"
    ]

  program = do
    whitespace
    Prog <$> many decl

  decl = {-context "Expect decl" $ -} alts [varDecl, statDecl]

  statDecl = DStat <$> stat

  varDecl = do
    key "var"
    x <- identifier
    eopt <- opt $ do key "="; expression
    key ";"
    pure (DVarDecl x eopt)

  identifier :: Par Identifier
  identifier = nibble $ do
    pos <- position
    x <- alpha
    xs <- many (alts [alpha,digit])
    let name = x:xs
    if name `notElem` keywords then pure (IdentifierX { pos, name }) else do
      let message = printf " at '%s': Expect variable name" name
      reject pos message

  stat =
    alts [printStat, expressionStat, blockStat]

  printStat = do
    key "print"
    e <- expression
    key ";"
    pure (SPrint e)

  expressionStat = do
    e <- expression
    key ";"
    pure (SExp e)

  blockStat = do
    key "{"
    xs <- many decl
    key "}"
    pure (SBlock xs)

  expression = assign

  assign = do
    e1 <- equality
    alts [ do pos <- position; key "="
              case e1 of
                EVar x1 -> do
                  e2 <- assign
                  pure (EAssign x1 e2)
                _ -> reject pos " at '=': Invalid assignment target"
         , pure e1
         ]

  equality = do
    pos <- position
    e1 <- comparison
    alts [ do op <- alts [ do key "=="; pure Equals
                         , do key "!="; pure NotEquals
                         ]
              e2 <- comparison
              pure (EBinary pos e1 op e2)
         , pure e1
         ]

  comparison = do
    pos <- position
    e1 <- term
    alts [ do op <- alts [ alts []
                         , do key "<="; pure LessEqual
                         , do key "<"; pure Less
                         , do key ">="; pure GreaterEqual
                         , do key ">"; pure Greater
                         ]
              e2 <- term
              pure (EBinary pos e1 op e2)
         , pure e1
         ]

  term = do
    pos <- position
    e1 <- factor
    alts [ do op <- alts [ do key "-"; pure Sub
                         , do key "+"; pure Add
                         ]
              e2 <- factor
              pure (EBinary pos e1 op e2)
         , pure e1
         ]

  factor = do
    pos <- position
    e1 <- unary
    alts [ do op <- alts [ do key "*"; pure Mul
                         , do key "/"; pure Div
                         ]
              e2 <- unary
              pure (EBinary pos e1 op e2)
         , pure e1
         ]

  unary = do
    pos <- position
    alts [ do op <- alts [ do key "-"; pure Negate
                         , do key "!"; pure Not
                         ]
              e <- unary
              pure (EUnary pos op e)
         , call
         ]

  call = primary

  primary :: Par Exp
  primary = alts
    [ ELit <$> literal
    , EVar <$> identifier
    , EGrouping <$> bracketed expression
    ]

  literal :: Par Lit
  literal = alts
    [ do key "nil"; pure LNil
    , do key "true"; pure (LBool True)
    , do key "false"; pure (LBool False)
    , LNumber <$> numberLit
    , LString <$> stringLit
    ]

  bracketed par = do
    key "("
    x <- par
    key ")"
    pure x

  numberLit = nibble (read <$> numberChars)
    where
      numberChars = do
        before <- some digit
        alts
          [ do lit '.'
               after <- some digit
               pure (before ++ "." ++ after)
          , pure before
          ]

  stringLit = context ": Unterminated string" $ nibble $ do
    doubleQuote
    x <- many stringLitChar
    doubleQuote
    pure x
    where
      doubleQuote = lit '"'
      stringLitChar = sat $ \c -> c /= '"'

  key s =
    if all isIdentifierChar s && s `notElem` keywords
    then error (printf "Add \"%s\" to keywords list" s)
    else nibble (noError (mapM_ lit s))
    where isIdentifierChar c = Char.isNumber c || Char.isAlpha c || c `elem` "'_"

  nibble par = do
    x <- par
    whitespace
    pure x

  whitespace = skip (alts [white1, commentToEol])

  white1 = alts [lit ' ', lit '\n', lit '\t']

  commentToEol :: Par ()
  commentToEol = do
    noError (do lit '/'; lit '/')
    skip notNL

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()

  alpha = sat Char.isAlpha
  digit = sat Char.isDigit
