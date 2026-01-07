module Runtime (Eff(..),Ref,runEffect) where

import Control.Monad (ap,liftM)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))

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
  WriteRef :: Ref a -> a -> Eff ()
  Clock :: Eff Double

type Ref a = IORef a

runEffect :: (String -> IO ()) -> Eff a -> IO (Either String a)
runEffect putOut eff = do
  start <- getTime Monotonic
  let
    loop :: Eff a -> (a -> IO (Either String b)) -> IO (Either String b)
    loop e = case e of
      Ret a -> \k -> k a
      Bind e f -> \k -> loop e $ \a -> loop (f a) k
      Print message -> \k -> do putOut message; k ()
      Error err -> \_ignoredK -> pure (Left err)
      NewRef v -> \k -> do newIORef v >>= k
      ReadRef r -> \k -> do readIORef r >>= k
      WriteRef r v -> \k -> do writeIORef r v >>= k
      Clock -> \k -> do
        now <- getTime Monotonic
        let TimeSpec{sec,nsec} = now - start
        let elapsed = fromIntegral sec + (fromIntegral nsec / fromIntegral gig)
        k elapsed

  loop eff (\a -> pure (Right a))

gig :: Int64
gig = 1_000_000_000
