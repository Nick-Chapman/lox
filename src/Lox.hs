module Lox (main) where

import Data.List (isSuffixOf)
import Par4 (Par,parse,lit,sat,alts,some,many,noError)
import System.Environment(getArgs)
import Text.Printf (printf)
import qualified Data.Char as Char

main :: IO ()
main = do
  getArgs >>= \case
    [] -> undefined --repl
    _:_:_ -> error "too any args"
    [arg1] -> do
      contents <- readFile arg1
      --sequence_ [ printf "%d: %s\n" i s | (i,s) <- zip [1::Int ..] (lines contents) ]
      let e = parse gram contents
      execute e
      pure ()

----------------------------------------------------------------------
-- Ast

data Prog
  = Prog [Decl]

data Decl
  = DStat Stat

data Stat
  = SPrint Exp

data Exp
  = ENumber Double
  | EBinary Exp Op2 Exp
  | EUnary Op1 Exp

data Op1 = Negate

data Op2 = Add | Sub

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
  SPrint e -> print (eval e)

----------------------------------------------------------------------
-- Evaluation

eval :: Exp -> Value
eval = \case
  ENumber n -> VNumber n
  EBinary e1 op e2 -> evalOp2 op (eval e1) (eval e2)
  EUnary op e -> evalOp1 op (eval e)
  where
    evalOp1 = \case
      Negate -> vnegate

    evalOp2 = \case
      Add -> vadd
      Sub -> vsub

    vadd v1 v2 = VNumber (getNumber v1 + getNumber v2)
    vsub v1 v2 = VNumber (getNumber v1 - getNumber v2)

    vnegate v1 = VNumber (negate $ getNumber v1)

----------------------------------------------------------------------
-- Values

data Value = VNumber Double

getNumber :: Value -> Double
getNumber = \case
  VNumber n -> n

instance Show Value where
  show = \case
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s

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
    alts [printStat]

  printStat = do
    key "print"
    e <- expression
    key ";"
    pure (SPrint e)

  expression = term

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
    [ ENumber <$> number
    , bracketed expression
    ]

  bracketed par = do
    key "("
    x <- par
    key ")"
    pure x

  number = nibble (read <$> numberChars)

  key chars = nibble (noError (mapM_ lit chars))

  nibble par = do
    x <- par
    whitespace
    pure x

  numberChars = do
    before <- digits
    alts
      [ do lit '.'
           after <- digits
           pure (before ++ "." ++ after)
      , pure before
      ]

  digits = some (sat Char.isDigit)

  whitespace = skip (alts [white1, commentToEol])

  white1 = alts [lit ' ', lit '\n', lit '\t']

  commentToEol = do
    lit '/'; lit '/'
    skip notNL
    lit '\n'

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()

  skip p = do _ <- many p; return ()
