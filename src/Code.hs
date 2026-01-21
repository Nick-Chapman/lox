module Code(Code(..),Const(..),export) where

import Data.ByteString.Internal (w2c)
import OP (Op)
import OP qualified
import Data.Word (Word8)

import GHC.Float (castDoubleToWord64)
import Data.Bits (shiftR,(.&.))


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
  ConstNumber d -> map w2c (bytesOfDoubleLittleEndian d) -- TODO: 8 bytes for the double
  ConstString _str -> undefined -- "S" ++ _str -- TODO: length or terminator

bytesOfDoubleLittleEndian :: Double -> [Word8]
bytesOfDoubleLittleEndian d = do
  let w = castDoubleToWord64 d
  [ fromIntegral (0xFF .&. (w `shiftR` (8*i))) | i <- [0..7] ]

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
