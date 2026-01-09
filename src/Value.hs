module Value (Value(..),Env(..),isTruthy,vequal) where

import Data.List (isSuffixOf)
import Text.Printf (printf)
import Runtime (Eff,Ref)
import Pos (Pos)
import Data.Map (Map)

data Env = Env (Map String (Ref Value))

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VFunc String ( {-globals-}Env -> Pos -> [Value] -> Eff Value)
  | VClass String (Map String Value)
  | VInstance String (Ref (Map String Value))

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VFunc name _  -> name
    VClass name _ -> name
    VInstance name _ -> name ++ " instance"

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True
  VFunc{} -> True
  VClass{} -> True
  VInstance{} -> True

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  _ -> False
