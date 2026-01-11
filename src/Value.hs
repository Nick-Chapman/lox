module Value (Value(..),Env(..),isTruthy,vequal) where

import Data.List (isSuffixOf)
import Text.Printf (printf)
import Runtime (Eff,Ref)
import Pos (Pos)
import Data.Map (Map)
import Ast (Identifier(..))

data Env = Env (Map Identifier (Ref Value))

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VFunc (Ref ()) String ( {-globals-}Env -> Pos -> [Value] -> Eff Value)
  | VInstance Identifier (Ref (Map Identifier Value))
  | VMethod (Ref () -> Value)

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VFunc _ name _  -> name
    VInstance Identifier{idString} _ -> idString ++ " instance"
    VMethod{} -> error "Show Value/VMethod"

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  VString{} -> True
  VNumber{} -> True
  VFunc{} -> True
  VInstance{} -> True
  VMethod{} -> error "isTruthy/VMethod"

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  (VFunc r1 _ _, VFunc r2 _ _) -> (r1 == r2)
  _ -> False
