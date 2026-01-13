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
  | SVarDecl Pos Identifier Exp
  | SFunDecl Func
  | SClassDecl Pos Identifier [Func]

data Func = Func
  { pos :: Pos
  , name :: Identifier
  , formals :: [(Pos,Identifier)]
  , body :: Stat
  }

data Exp
  = ELit Lit
  | EGrouping Exp
  | EBinary Pos Exp Op2 Exp
  | EUnary Pos Op1 Exp
  | EVar Pos Identifier
  | EThis Pos
  | EAssign Pos Identifier Exp
  | ELogicalAnd Exp Exp
  | ELogicalOr Exp Exp
  | ECall Pos Exp [Exp]
  | EGetProp Pos Exp Identifier
  | ESetProp Pos Exp Identifier Exp

data Lit
  = LNil
  | LBool Bool
  | LNumber Double
  | LString String

data Op1 = Negate | Not

data Op2 = Add | Sub | Mul | Div | Equals | NotEquals | Less | LessEqual | Greater | GreaterEqual

data Identifier = Identifier { idString :: String } deriving (Eq,Ord)
