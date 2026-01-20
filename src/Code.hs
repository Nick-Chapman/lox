module Code(Code(..),Const(..),export) where

import Data.ByteString.Internal (w2c)
import OP (Op)
import OP qualified

data Code = Code
  { constants :: [Const]
  , chunk :: [Op]
  }

data Const = ConstNumber Double | ConstString String
  deriving Show

export :: Code -> String
export Code{constants,chunk=ops} = do
  let n = length constants
  if n > 255 then error "too many constants" else -- TODO: aligment?
    "LoxCode" ++ [ w2c (fromIntegral n) ] ++
    concat [ codeConst c | c <- constants ] ++
    [ codeOp op | op <- ops ]

codeConst :: Const -> String
codeConst = \case
  ConstNumber _d -> "D" -- TODO: 8 bytes for the double
  ConstString str -> "S" ++ str -- TODO: length or terminator

codeOp :: Op -> Char
codeOp = \case
  OP.ADD                -> '+'
  OP.CONSTANT           -> 'c'
  OP.DIVIDE             -> '/'
  OP.EQUAL              -> '='
  OP.FALSE              -> 'f'
  OP.GET_LOCAL          -> 'g'
  OP.GREATER            -> '>'
  OP.JUMP               -> 'j'
  OP.JUMP_IF_FALSE      -> 'b' -- Branch
  OP.LESS               -> '<'
  OP.MULTIPLY           -> '*'
  OP.NEGATE             -> '~'
  OP.NIL                -> 'n'
  OP.NOT                -> '!'
  OP.POP                -> 'd' -- Drop
  OP.PRINT              -> 'p'
  OP.SET_LOCAL          -> 's'
  OP.SUBTRACT           -> '-'
  OP.TRUE               -> 't'
  OP.ARG byte           -> w2c byte
