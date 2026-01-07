module Value (Value(..),isTruthy,vequal) where

import Data.List (isSuffixOf)
import Text.Printf (printf)
import Runtime (Eff)
import Par4 (Pos)

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VFunc String (Pos -> [Value] -> Eff Value)

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VFunc name _  -> printf "<fn %s>" name

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True
  VFunc{} -> True

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  _ -> False
