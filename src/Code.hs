module Code(Code(..),export) where

import Data.ByteString.Internal (w2c,c2w)
import OP (Op)
import OP qualified
import Data.Word (Word8,Word16,Word64)

import GHC.Float (castDoubleToWord64)
import Data.Bits (shiftR,(.&.))

export :: Code -> String
export = map w2c . encode

data Code = Code
  { numbers :: [Double]
  , strings :: [String]
  , chunk :: [Op]
  } deriving Show

encode :: Code -> [Word8]
encode Code{numbers,strings,chunk=ops} =
  map c2w "LOX---" ++
  [ byteLength "too many constant numbers" (length numbers) ] ++
  [ byteLength "too many constant strings" (length strings) ] ++
  concat [ encodeDouble n | n <- numbers ] ++
  concat [ encodeWord16 (shortLength "string too long" (length s)) | s <- strings ] ++
  concat [ map c2w (s ++ ['\0']) | s <- strings ] ++
  [ encodeOp op | op <- ops ]

byteLength :: String -> Int -> Word8
byteLength tag n = if n >= 256 then error tag else fromIntegral n

shortLength :: String -> Int -> Word16
shortLength tag n = if n >= 256*256 then error tag else fromIntegral n


encodeDouble :: Double -> [Word8]
encodeDouble = encodeWord64 . castDoubleToWord64

encodeWord64 :: Word64 -> [Word8] -- little endian
encodeWord64 w = [ fromIntegral (0xFF .&. (w `shiftR` (8*i))) | i <- [0..7] ]

encodeWord16 :: Word16 -> [Word8] -- little endian
encodeWord16 w = [ fromIntegral (0xFF .&. (w `shiftR` (8*i))) | i <- [0..1] ]

encodeOp :: Op -> Word8
encodeOp = c2w . \case
  OP.ADD                -> '+'
  OP.CONSTANT_NUM       -> 'c'
  OP.CONSTANT_STR       -> '"'
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
