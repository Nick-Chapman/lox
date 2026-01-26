module OP(Op(..)) where

import Data.Word (Word8)

data Op
  = ADD
  | CONSTANT_NUM
  | CONSTANT_STR
  | CONSTANT_FUNC
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
  | CALL
  | RETURN

  | GET_UPVALUE
  | SET_UPVALUE
  | CLOSURE

  | ARG Word8
  deriving Show
