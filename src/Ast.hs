module Ast (Prog(..),Decl(..),Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..)) where

import Par4 (Pos)

data Prog
  = Prog [Decl]

data Decl
  = DStat Stat
  | DVarDecl Identifier (Maybe Exp)

data Stat
  = SExp Exp
  | SPrint Exp
  | SBlock [Decl]
  | SIf Exp Stat Stat

data Exp
  = ELit Lit
  | EGrouping Exp
  | EBinary Pos Exp Op2 Exp
  | EUnary Pos Op1 Exp
  | EVar Identifier
  | EAssign Identifier Exp
  | ELogicalAnd Exp Exp
  | ELogicalOr Exp Exp

data Lit
  = LNil
  | LBool Bool
  | LNumber Double
  | LString String

data Op1 = Negate | Not

data Op2 = Add | Sub | Mul | Div | Equals | NotEquals | Less | LessEqual | Greater | GreaterEqual

data Identifier = Identifier { pos :: Pos, name :: String }
