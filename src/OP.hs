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
  -- | LOOP

  | CALL
  | CLOSURE
  | INDIRECT
  | RETURN

  | ARG Word8
  deriving Show


encode :: Op -> Word8
encode = c2w . \case

  OP.NUMBER             -> 'c'
  OP.STRING             -> '"'
  OP.NIL                -> 'n'
  OP.TRUE               -> 't'
  OP.FALSE              -> 'f'

  OP.POP                -> 'd'
  OP.GET_LOCAL          -> 'g'
  OP.SET_LOCAL          -> 's'
  OP.GET_UPVALUE        -> 'G'
  OP.SET_UPVALUE        -> 'S'

  OP.EQUAL              -> '='
  OP.GREATER            -> '>'
  OP.LESS               -> '<'
  OP.ADD                -> '+'
  OP.SUBTRACT           -> '-'
  OP.MULTIPLY           -> '*'
  OP.DIVIDE             -> '/'
  OP.NOT                -> '!'
  OP.NEGATE             -> '~'

  OP.PRINT              -> 'p'
  OP.JUMP               -> 'j'
  OP.JUMP_IF_FALSE      -> 'b'
  -- OP.LOOP

  OP.CALL               -> 'C'
  OP.CLOSURE            -> 'F'
  OP.INDIRECT           -> 'I'
  OP.RETURN             -> 'R'

  OP.ARG byte           -> w2c byte
