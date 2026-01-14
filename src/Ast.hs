module Ast (Stat(..),Func(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..)) where

import Pos (Pos)

data Stat
  = SExp Exp
  | SPrint Exp
  | SBlock [Stat]
  | SIf Exp Stat Stat
  | SWhile Exp Stat
  | SFor (Stat,Exp,Stat) Stat
  | SReturn Pos (Maybe Exp)
  | SVarDecl Identifier Exp
  | SFunDecl Func
  | SClassDecl Identifier (Maybe Identifier) [Func]

data Func = Func
  { name :: Identifier
  , formals :: [Identifier]
  , statements :: [Stat]
  }

data Exp
  = ELit Lit
  | EGrouping Exp
  | EBinary Pos Exp Op2 Exp
  | EUnary Pos Op1 Exp
  | EVar Identifier
  | ESuperVar Pos Identifier
  | EThis Pos
  | EAssign Identifier Exp
  | ELogicalAnd Exp Exp
  | ELogicalOr Exp Exp
  | ECall Pos Exp [Exp]
  | EGetProp Exp Identifier
  | ESetProp Exp Identifier Exp

data Lit
  = LNil
  | LBool Bool
  | LNumber Double
  | LString String

data Op1 = Negate | Not

data Op2 = Add | Sub | Mul | Div | Equals | NotEquals | Less | LessEqual | Greater | GreaterEqual

data Identifier = Identifier { pos :: Pos, name :: String }
