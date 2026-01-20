module OP(Op(..)) where

import Data.Word (Word8)

data Op
  = CONSTANT
  | NIL
  | TRUE
  | FALSE
  | POP
  | GET_LOCAL
  | SET_LOCAL
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
  | ARG Word8
  deriving Show
