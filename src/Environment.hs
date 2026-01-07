module Environment (Env,emptyEnv,insertEnv,lookupEnv) where

import Ast (Identifier(..))
import Data.Map (Map)
import Data.Map qualified as Map
import Runtime (Ref)

-- Polymorphic Env to avoid module cycle with Value
data Env v = Env (Map String (Ref v))

emptyEnv :: Env v
emptyEnv = Env Map.empty

insertEnv :: Env v -> Identifier -> Ref v -> Env v
insertEnv (Env m) Identifier{name} r = do
  Env (Map.insert name r m)

lookupEnv :: Env v -> Identifier -> Maybe (Ref v)
lookupEnv (Env m) Identifier{name} = Map.lookup name m
