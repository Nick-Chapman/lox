module Code(Code(..),export) where

import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (w2c,c2w)
import Data.Word (Word8,Word16,Word64)
import GHC.Float (castDoubleToWord64)
import OP (Op)
import OP qualified (encode)
import Pos (Pos)

export :: Code -> String
export = map w2c . encode

data Code = Code
  { numbers :: [Double]
  , strings :: [String]
  , chunk :: [(Pos,Op)]
  } deriving Show

encode :: Code -> [Word8]
encode Code{numbers,strings,chunk=ops} =
  map c2w "LOX---" ++
  [ byteLength "too many constant numbers" (length numbers) ] ++
  [ byteLength "too many constant strings" (length strings) ] ++
  concat [ encodeDouble n | n <- numbers ] ++
  concat [ encodeWord16 (shortLength "string too long" (length s)) | s <- strings ] ++
  concat [ map c2w (s ++ ['\0']) | s <- strings ] ++
  [ OP.encode op | (_pos_USEME,op) <- ops ]

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
