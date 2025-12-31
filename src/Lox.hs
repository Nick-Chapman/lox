module Lox (main) where

import Data.List (isSuffixOf)
import Par4 (Par,parse,lit,sat,alts,many,noError,int)
import System.Environment(getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> undefined --repl
    _:_:_ -> error "too any args"
    [arg1] -> do
      contents <- readFile arg1
      --sequence_ [ printf "%d: %s\n" i s | (i,s) <- zip [1::Int ..] (lines contents) ]
      let e = parse gram contents
      --print e
      execute e
      pure ()


execute :: Exp -> IO ()
execute e = print (eval e)

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
      let s = show n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s

----------------------------------------------------------------------
-- Ast

data Exp
  = ENumber Double
  | EBinary Exp Op2 Exp
  | EUnary Op1 Exp

data Op1 = Negate
data Op2 = Add | Sub

----------------------------------------------------------------------
-- PP

instance Show Exp where
  show = \case
    ENumber n -> show n
    EBinary e1 op e2 -> printf "(%s %s %s)" (show e1) (show op) (show e2)
    EUnary op e -> printf "(%s %s)" (show op) (show e)

instance Show Op1 where show = \case Negate -> "-"
instance Show Op2 where show = \case Add -> "+"; Sub -> "-"

----------------------------------------------------------------------
-- Parse

gram :: Par Exp
gram = program where

  program = do
    whitespace
    expression

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

  number :: Par Double
  number = nibble (fromIntegral <$> int) -- TODO: decimals

  bracketed par = do
    key "("
    x <- par
    key ")"
    pure x

  key chars = nibble (noError (mapM_ lit chars))

  nibble par = do
    x <- par
    whitespace
    pure x

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
