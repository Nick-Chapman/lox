module OP(Op(..)) where

data Op
  = CONSTANT_NUM Int
  | CONSTANT_STR Int -- TODO: avoid stratification on constant type
  | NIL
  | TRUE
  | FALSE
  | POP
  | GET_LOCAL Int
  | SET_LOCAL Int
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
  deriving Show
