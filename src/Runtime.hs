module Runtime (Eff(..),Ref,runEffect) where

import Control.Monad (ap,liftM)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import GHC.Int (Int64)
import Pos (Pos,showPos)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import Text.Printf (printf)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = Ret; (<*>) = ap
instance Monad Eff where (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Print :: String -> Eff ()
  NewRef :: a -> Eff (Ref a)
  ReadRef :: Ref a -> Eff a
  WriteRef :: Ref a -> a -> Eff ()
  Clock :: Eff Double
  WithFunctionCall :: Pos -> String -> Eff a -> Eff a
  Error :: Pos -> String -> Eff a

type Ref a = IORef a

data Down = Down { caller :: String, backtrace :: [(Pos,String)] }

runEffect :: (String -> IO ()) -> Eff a -> IO (Either String a)
runEffect putOut eff = do
  start <- getTime Monotonic
  let
    loop :: Down -> Eff a -> (a -> IO (Either String b)) -> IO (Either String b)
    loop d e = case e of
      Ret a -> \k -> k a
      Bind e f -> \k -> loop d e $ \a -> loop d (f a) k
      Print message -> \k -> do putOut message; k ()
      NewRef v -> \k -> do newIORef v >>= k
      ReadRef r -> \k -> do readIORef r >>= k
      WriteRef r v -> \k -> do writeIORef r v >>= k
      Clock -> \k -> do
        now <- getTime Monotonic
        let TimeSpec{sec,nsec} = now - start
        let elapsed = fromIntegral sec + (fromIntegral nsec / fromIntegral gig)
        k elapsed
      WithFunctionCall pos caller e -> \k -> do
        let Down{caller = prevCalller, backtrace} = d
        let d' = Down { caller, backtrace = (pos,prevCalller) : backtrace }
        loop d' e k
      Error pos err -> \_ignoredK -> do
        let Down{caller,backtrace} = d
        let bt = (pos,caller) : backtrace
        pure $ Left $ printf "%s%s" err $
          concat [ printf "\n%s in %s" (showPos pos) context :: String
                 | (pos,context) <- bt ]

  let down0 = Down { caller = "script", backtrace = [] }
  loop down0 eff (\a -> pure (Right a))

gig :: Int64
gig = 1_000_000_000
