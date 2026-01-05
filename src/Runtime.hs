module Runtime (Eff(..),Ref,runEffect) where

import Control.Monad (ap,liftM)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = Ret; (<*>) = ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Print :: String -> Eff ()
  Error :: String -> Eff a
  NewRef :: a -> Eff (Ref a)
  ReadRef :: Ref a -> Eff a
  WriteRef :: a -> Ref a -> Eff ()

type Ref a = IORef a

runEffect :: (String -> IO ()) -> Eff () -> IO (Maybe String)
runEffect putOut eff = loop eff (\() -> pure Nothing)
  where
    loop :: Eff a -> (a -> IO (Maybe String)) -> IO (Maybe String)
    loop e = case e of
      Ret a -> \k -> k a
      Bind e f -> \k -> loop e $ \a -> loop (f a) k
      Print message -> \k -> do putOut message; k ()
      Error err -> \_ignoredK -> pure (Just err)
      NewRef v -> \k -> do newIORef v >>= k
      ReadRef r -> \k -> do readIORef r >>= k
      WriteRef v r -> \k -> do writeIORef r v >>= k
