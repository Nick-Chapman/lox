module Lox (main) where

import Data.List (isSuffixOf)
import Par4 (Par,parse,lit,sat,alts,some,many,noError,skip)
import System.Environment(getArgs)
import System.FilePath (takeFileName)
import Text.Printf (printf)
import qualified Data.Char as Char

main :: IO ()
main = do
  getArgs >>= \case
    [] -> undefined --repl
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

data Op1 = Negate

data Op2 = Add | Sub | Equal

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
  SExp e -> do _ <- pure $ eval e; pure ()
  SPrint e -> print (eval e)

----------------------------------------------------------------------
-- Evaluation

eval :: Exp -> Value
eval = \case
  ELit x -> evalLit x
  EBinary e1 op e2 -> evalOp2 op (eval e1) (eval e2)
  EUnary op e -> evalOp1 op (eval e)
  where
    evalLit = \case
      LNil{} -> VNil
      LBool b -> VBool b
      LNumber n -> VNumber n
      LString s -> VString s

    evalOp1 = \case
      Negate -> vnegate

    evalOp2 = \case
      Add -> vadd
      Sub -> vsub
      Equal -> \v1 v2 -> VBool (vequal v1 v2)

    vadd v1 v2 = VNumber (getNumber v1 + getNumber v2)
    vsub v1 v2 = VNumber (getNumber v1 - getNumber v2)

    vnegate v1 = VNumber (negate $ getNumber v1)

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

getNumber :: Value -> Double
getNumber = \case
  VNumber n -> n
  v -> error (show ("getNumber",v))

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> show s

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
    alts [ do op <- alts [ do key "=="; pure Equal
                         ]
              e2 <- comparison
              pure (EBinary e1 op e2)
         , pure e1
         ]

  comparison = term

  term = do
    e1 <- factor
    alts [ do op <- alts [ do key "-"; pure Sub
                         , do key "+"; pure Add
                         ]
              e2 <- factor
              pure (EBinary e1 op e2)
         , pure e1
         ]

  factor = unary

  unary = alts
    [ do op <- alts [ do key "-"; pure Negate ]
         e <- primary
         pure (EUnary op e)
    , primary
    ]

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

  stringLit = nibble $ do
    doubleQuote
    x <- many stringLitChar
    doubleQuote
    pure x

  doubleQuote = lit '"'

  stringLitChar = sat $ \c -> c /= '"'

  key chars = nibble (noError (mapM_ lit chars))

  nibble par = do
    x <- par
    whitespace
    pure x

  numberChars = do
    before <- some digit
    alts
      [ do lit '.'
           after <- some digit
           pure (before ++ "." ++ after)
      , pure before
      ]

  digit = sat Char.isDigit

  whitespace = skip (alts [white1, commentToEol])

  white1 = alts [lit ' ', lit '\n', lit '\t']

  commentToEol = do
    lit '/'; lit '/'
    skip notNL
    lit '\n'

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()
