module OP(Op(..)) where

data Op
  = CONSTANT Int
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
