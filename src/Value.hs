module Value (Value(..),isTruthy,vequal) where

import Data.List (isSuffixOf)
import Text.Printf (printf)
import Ast (Identifier(..),Stat)
import Environment (Env)

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VClosure
    { fname :: Identifier
    , formals :: [Identifier]
    , body :: Stat
    , env :: Env Value
    }

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VClosure{fname=Identifier{name}} -> printf "<fn %s>" name

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True
  VClosure{} -> True

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  _ -> False
