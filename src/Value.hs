module Value (Value(..),Env(..),Closure(..),Method(..),BoundMethod(..),ClassValue(..),InstanceValue(..),isTruthy,vequal) where

import Data.List (isSuffixOf)
import Data.Map (Map)
import Pos (Pos)
import Runtime (Eff,Ref)
import Text.Printf (printf)

data Env = Env (Map String (Ref Value))

data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VNativeClockFun
  | VFunc Closure
  | VBoundMethod BoundMethod
  | VClass ClassValue
  | VInstance InstanceValue

data ClassValue = ClassValue { classIdentity :: Ref ()
                             , className :: String
                             , methodMap :: Map String Method }

data Method = Method ( {-this-}InstanceValue -> Eff BoundMethod )

data InstanceValue = InstanceValue { myClass :: ClassValue
                                   , fields :: Ref (Map String Value) }

data BoundMethod = BoundMethod { identity :: Ref ()
                               , closure :: Closure }

data Closure = Closure { name :: String
                       , func :: {-self-}Value -> {-globals-}Env -> Pos -> [Value] -> Eff Value }

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VNativeClockFun -> "<native fn>"
    VFunc Closure{name}  -> printf "<fn %s>" name
    VBoundMethod BoundMethod{closure=Closure{name}} -> printf "<fn %s>" name
    VClass ClassValue{className} -> className
    VInstance InstanceValue{myClass=ClassValue{className}} -> className ++ " instance"

isTruthy :: Value -> Bool
isTruthy = \case
  VNil -> False
  VBool b -> b
  _ -> True

vequal :: Value -> Value -> Bool
vequal v1 v2 = case (v1,v2) of
  (VNil,VNil) -> True
  (VBool b1, VBool b2) -> b1 == b2
  (VNumber n1, VNumber n2) -> n1 == n2
  (VString s1, VString s2) -> s1 == s2
  (VBoundMethod BoundMethod{identity=r1}, VBoundMethod BoundMethod{identity=r2}) -> (r1 == r2)
  (VClass ClassValue{classIdentity=r1}, VClass ClassValue{classIdentity=r2}) -> (r1 == r2)
  _ -> False
