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

  -- TODO: change it to 24bit in 3 following byte args
  | JUMP Int --forward or backward
  | JUMP_IF_FALSE Int
  deriving Show
