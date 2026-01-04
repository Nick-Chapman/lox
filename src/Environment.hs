module Environment (Env,env0,insertEnv,readEnv,assignEnv) where

import Ast (Identifier(..))
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Runtime (runtimeError)
import Text.Printf (printf)
import Value (Value)

data Env = Env (Map String (IORef Value))

env0 :: Env
env0 = Env Map.empty

insertEnv :: Env -> Identifier -> Value -> IO Env
insertEnv (Env m) Identifier{name} v = do
  r <- newIORef v
  pure $ Env (Map.insert name r m)

lookupEnv :: Env -> Identifier -> IO (IORef Value)
lookupEnv (Env m) Identifier{name,pos} =
  case Map.lookup name m of
    Just r -> pure r
    Nothing -> runtimeError pos (printf "Undefined variable '%s'." name)

readEnv :: Env -> Identifier -> IO Value
readEnv env x = do
  r <- lookupEnv env x
  readIORef r

assignEnv :: Env -> Identifier -> Value -> IO ()
assignEnv env x v = do
  r <- lookupEnv env x
  writeIORef r v
