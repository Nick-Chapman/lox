module OP(Op(..),encode,printableOffset) where

import Data.ByteString.Internal (w2c,c2w)
import Data.Word (Word8)

printableOffset :: Int
printableOffset = 0

data Op

  = NUMBER
  | STRING

  | NIL
  | TRUE
  | FALSE

  | POP
  | GET_LOCAL_REF
  | GET_UPVALUE_REF

  | INDIRECT
  | DEREF
  | ASSIGN

  | EQUAL
  | GREATER
  | LESS
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | NOT
  | NEGATE

  | PRINT

  | JUMP
  | JUMP_IF_FALSE
  | LOOP

  | CALL
  | CLOSURE
  | CLOSURE_noind
  | RETURN

  | CLOCK

  | ARG Int
  deriving Show


encode :: Op -> Word8
encode = c2w . \case

  OP.NUMBER             -> '#'
  OP.STRING             -> '$'

  OP.NIL                -> 'z'
  OP.TRUE               -> 't'
  OP.FALSE              -> 'f'

  OP.POP                -> '_'
  OP.GET_LOCAL_REF      -> '/'
  OP.GET_UPVALUE_REF    -> '\\'

  OP.INDIRECT           -> '&'
  OP.DEREF              -> '*'
  OP.ASSIGN             -> '='

  OP.EQUAL              -> 'e'
  OP.GREATER            -> 'g'
  OP.LESS               -> 'l'
  OP.ADD                -> 'a'
  OP.SUBTRACT           -> 's'
  OP.MULTIPLY           -> 'm'
  OP.DIVIDE             -> 'd'
  OP.NOT                -> 'n'
  OP.NEGATE             -> 'i'

  OP.PRINT              -> 'p'

  OP.JUMP               -> 'J'
  OP.JUMP_IF_FALSE      -> 'B'
  OP.LOOP               -> 'L'

  OP.CALL               -> 'C'
  OP.CLOSURE            -> 'F'
  OP.CLOSURE_noind      -> 'G'
  OP.RETURN             -> 'R'

  OP.CLOCK              -> '@'

  OP.ARG byte -> do
    if byte < 0 || byte > 255 then error (show ("encode/OP.ARG",byte)) else
      w2c (fromIntegral (printableOffset + byte))
