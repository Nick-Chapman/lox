module Environment (Env,emptyEnv,insertEnv,readEnv,assignEnv) where

import Ast (Identifier(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Runtime (Eff(NewRef,ReadRef,WriteRef),Ref)
import Value (Value)

data Env = Env (Map String (Ref Value))

emptyEnv :: Env
emptyEnv = Env Map.empty

insertEnv :: Env -> Identifier -> Value -> Eff Env
insertEnv (Env m) Identifier{name} v = do
  r <- NewRef v
  pure $ Env (Map.insert name r m)

lookupEnv :: Env -> Identifier -> Maybe (Ref Value)
lookupEnv (Env m) Identifier{name} = Map.lookup name m

readEnv :: Env -> Identifier -> Maybe (Eff Value)
readEnv env x =
  ReadRef <$> lookupEnv env x

assignEnv :: Env -> Identifier -> Value -> Maybe (Eff ())
assignEnv env x v =
  WriteRef v <$> lookupEnv env x
