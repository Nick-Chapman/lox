module OP(Op(..),encode) where

import Data.ByteString.Internal (w2c,c2w)

import Data.Word (Word8)

data Op

  = NUMBER
  | STRING
  | NIL
  | TRUE
  | FALSE

  | POP
  | GET_LOCAL
  | SET_LOCAL
  | GET_UPVALUE
  | SET_UPVALUE

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
  | INDIRECT
  | RETURN

  | CLOCK

  | ARG Word8
  deriving Show


encode :: Op -> Word8
encode = c2w . \case

  OP.NUMBER             -> '#'
  OP.STRING             -> '$'

  OP.NIL                -> 'z'
  OP.TRUE               -> 't'
  OP.FALSE              -> 'f'

  OP.POP                -> '_'
  OP.GET_LOCAL          -> '.'
  OP.SET_LOCAL          -> ':'
  OP.GET_UPVALUE        -> ','
  OP.SET_UPVALUE        -> ';'

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
  OP.INDIRECT           -> '&'
  OP.RETURN             -> 'R'

  OP.CLOCK              -> '@'

  OP.ARG byte           -> w2c byte
