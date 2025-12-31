module Lox (main) where

import Data.List (isSuffixOf)
import Par4 (Par,parse,lit,sat,alts,some,many,noError,skip)
import System.Environment(getArgs)
import System.FilePath (takeFileName)
import Text.Printf (printf)
import qualified Data.Char as Char
import System.Exit(ExitCode(..),exitWith)
import System.IO (stderr,hFlush,hPutStrLn)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> error "repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- readFile filename
      --sequence_ [ printf "%d: %s\n" i s | (i,s) <- zip [1::Int ..] (lines contents) ]
      case parse (takeFileName filename) gram contents of
        Right prog ->
          execute prog
        Left err ->
          print err
      pure ()

----------------------------------------------------------------------
-- Ast

data Prog
  = Prog [Decl]

data Decl
  = DStat Stat

data Stat
  = SExp Exp
  | SPrint Exp

data Exp
  = ELit Lit
  | EBinary Exp Op2 Exp
  | EUnary Op1 Exp

data Lit
  = LNil
  | LBool Bool
  | LNumber Double
  | LString String

data Op1 = Negate | Not

data Op2 = Add | Sub | Mul | Div | Equals | NotEquals | Less | LessEqual | Greater | GreaterEqual

----------------------------------------------------------------------
-- Execution

execute :: Prog -> IO ()
execute = \case
  Prog decs -> mapM_ execD decs

execD :: Decl -> IO ()
execD = \case
  DStat stat -> execS stat

execS :: Stat -> IO ()
execS = \case
  SExp e -> do
    _ <- eval e
    pure ()
  SPrint e -> do
    v <- eval e
    print v

----------------------------------------------------------------------
-- Evaluation

eval :: Exp -> IO Value
eval = \case
  ELit x -> pure (evalLit x)
  EBinary e1 op e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    evalOp2 v1 v2 op
  EUnary op e -> do
    v <- eval e
    evalOp1 op v
  where
    evalLit = \case
      LNil{} -> VNil
      LBool b -> VBool b
      LNumber n -> VNumber n
      LString s -> VString s

    evalOp1 = \case
      Negate -> vnegate
      Not -> \v -> pure (VBool (not (isTruthy v)))

    evalOp2 v1 v2 = \case
      Add -> vadd (v1,v2)
      Sub -> VNumber <$> binary (-) v1 v2
      Mul -> VNumber <$> binary (*) v1 v2
      Div -> VNumber <$> binary (/) v1 v2
      Equals -> pure (VBool (vequal v1 v2))
      NotEquals -> pure (VBool (not (vequal v1 v2)))
      Less -> VBool <$> binary (<) v1 v2
      LessEqual -> VBool <$> binary (<=) v1 v2
      Greater -> VBool <$> binary (>) v1 v2
      GreaterEqual -> VBool <$> binary (>=) v1 v2

    binary f v1 v2 = do
      n1 <- getNumber "Operands must be numbers." v1
      n2 <- getNumber "Operands must be numbers." v2
      pure (f n1 n2)

vnegate :: Value -> IO Value
vnegate v1 = do
  n1 <- getNumber "Operand must be a number." v1
  pure (VNumber (negate n1))

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True

getNumber :: String -> Value -> IO Double
getNumber message = \case
  VNumber n -> pure n
  _ -> abort message

vadd :: (Value,Value) -> IO Value
vadd = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> abort "Operands must be two numbers or two strings."

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  _ -> False

abort :: String -> IO a
abort mes = do
  putErr mes
  putErr "[line 1]" -- hack -- TODO: pass line number from caller
  exitWith (ExitFailure 70)

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr
  pure ()

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

  program = do
    whitespace
    Prog <$> many decl

  decl :: Par Decl =
    DStat <$> stat

  stat :: Par Stat =
    alts [printStat, expressionStat]

  printStat = do
    key "print"
    e <- expression
    key ";"
    pure (SPrint e)

  expressionStat = do
    e <- expression
    key ";"
    pure (SExp e)

  expression = equality

  equality = do
    e1 <- comparison
    alts [ do op <- alts [ do key "=="; pure Equals
                         , do key "!="; pure NotEquals
                         ]
              e2 <- comparison
              pure (EBinary e1 op e2)
         , pure e1
         ]

  comparison = do
    e1 <- term
    alts [ do op <- alts [ alts []
                         , do key "<="; pure LessEqual
                         , do key "<"; pure Less
                         , do key ">="; pure GreaterEqual
                         , do key ">"; pure Greater
                         ]
              e2 <- term
              pure (EBinary e1 op e2)
         , pure e1
         ]

  term = do
    e1 <- factor
    alts [ do op <- alts [ do key "-"; pure Sub
                         , do key "+"; pure Add
                         ]
              e2 <- factor
              pure (EBinary e1 op e2)
         , pure e1
         ]

  factor = do
    e1 <- unary
    alts [ do op <- alts [ do key "*"; pure Mul
                         , do key "/"; pure Div
                         ]
              e2 <- unary
              pure (EBinary e1 op e2)
         , pure e1
         ]

  unary = alts
    [ do op <- alts [ do key "-"; pure Negate
                    , do key "!"; pure Not
                    ]
         e <- unary
         pure (EUnary op e)
    , call
    ]

  call = primary

  primary :: Par Exp
  primary = alts
    [ ELit <$> literal
    , bracketed expression
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
      digit = sat Char.isDigit

  stringLit = nibble $ do
    doubleQuote
    x <- many stringLitChar
    doubleQuote
    pure x
    where
      doubleQuote = lit '"'
      stringLitChar = sat $ \c -> c /= '"'

  key chars = nibble (noError (mapM_ lit chars))

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
    lit '\n'

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()
