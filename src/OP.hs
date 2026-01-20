module OP(Op(..)) where

import Data.Word (Word8)

data Op
  = ADD
  | CONSTANT
  | DIVIDE
  | EQUAL
  | FALSE
  | GET_LOCAL
  | GREATER
  | JUMP
  | JUMP_IF_FALSE
  | LESS
  | MULTIPLY
  | NEGATE
  | NIL
  | NOT
  | POP
  | PRINT
  | SET_LOCAL
  | SUBTRACT
  | TRUE

  | ARG Word8
  deriving Show
