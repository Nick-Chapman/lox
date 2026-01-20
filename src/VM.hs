module VM (Code(..),Const(..), runCode) where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Map qualified as Map
import OP (Op)
import OP qualified
import Pos (Pos,initPos)
import Runtime (Eff)
import Runtime qualified (Eff(Print))
import Value (Value(..),binary,vadd,isTruthy,vnegate,vequal)

data Code = Code
  { constants :: [Const]
  , chunk :: [Op]
  }

data Const = ConstNumber Double | ConstString String
  deriving Show

pos0 :: Pos -- TODO: avoid dummy hack
pos0 = initPos

runCode :: Code -> Eff ()
runCode code = do
  runVM code fetchDispatchLoop

fetchDispatchLoop :: VM ()
fetchDispatchLoop = do
  Fetch >>= \case
    Nothing -> pure () -- halt
    Just op -> do
      execOp op
      fetchDispatchLoop

execOp :: Op -> VM ()
execOp = \case
  OP.CONSTANT i -> do
    v <- GetConst i
    Push v

  OP.NIL -> Push VNil
  OP.TRUE -> Push (VBool True)
  OP.FALSE -> Push (VBool False)
  OP.POP -> do _ <- Pop; pure ()

  OP.GET_LOCAL i -> do
    v <- GetSlot i
    Push v

  OP.SET_LOCAL i -> do
    v <- Pop; Push v -- peek
    SetSlot i v

  OP.EQUAL -> do
    v2 <- Pop
    v1 <- Pop
    Push (VBool (vequal v1 v2))

  OP.GREATER -> execBinary VBool (>)
  OP.LESS -> execBinary VBool (<)

  OP.ADD -> do
    v2 <- Pop
    v1 <- Pop
    v <- Effect (vadd pos0 (v1,v2))
    Push v

  OP.SUBTRACT -> execBinary VNumber (-)
  OP.MULTIPLY -> execBinary VNumber (*)
  OP.DIVIDE -> execBinary VNumber (/)

  OP.NOT -> do
    v <- Pop
    Push (VBool (not (isTruthy v)))

  OP.NEGATE -> do
    v <- Pop
    v' <- Effect (vnegate pos0 v)
    Push v'

  OP.PRINT -> do
    v <- Pop
    Effect (Runtime.Print (show v))

execBinary :: (a -> Value) -> (Double -> Double -> a) -> VM ()
execBinary mk f = do
  v2 <- Pop
  v1 <- Pop
  n <- Effect (binary pos0 f v1 v2)
  Push (mk n)

instance Functor VM where fmap = liftM
instance Applicative VM where pure = Ret; (<*>) = ap
instance Monad VM where (>>=) = Bind

data VM a where
  Ret :: a -> VM a
  Bind :: VM a -> (a -> VM b) -> VM b
  Effect :: Eff a -> VM a
  Push :: Value -> VM ()
  Pop :: VM Value
  GetSlot :: Int -> VM Value
  SetSlot :: Int -> Value -> VM ()
  GetConst :: Int -> VM Value

  Fetch :: VM (Maybe Op)

runVM :: Code -> VM () -> Eff ()
runVM Code{constants,chunk} m = loop state0 m kFinal
  where
    progSize = length chunk
    kFinal () _ = pure ()
    loop :: State -> VM a -> (a -> State -> Eff ()) -> Eff ()
    loop s = \case
      Ret a -> \k -> k a s
      Bind m f -> \k -> loop s m $ \a s -> loop s (f a) k
      Effect eff -> \k -> do a <- eff; k a s
      Push v -> \k -> do
        let State{depth,stack} = s
        k () s { depth = 1 + depth, stack = Map.insert depth v stack }
      Pop -> \k -> do
        let State{depth,stack} = s
        let v = maybe (error"Pop") id $ Map.lookup (depth-1) stack
        k v s { depth = depth - 1, stack = Map.delete (depth-1) stack }
      GetSlot i -> \k -> do
        let State{stack} = s
        let v = maybe (error "GetSlot") id $ Map.lookup i stack
        k v s
      SetSlot i v -> \k -> do
        let State{stack} = s
        k () s { stack = Map.insert i v stack }
      GetConst i -> \k -> do
        let v = case constants !! i of
                  ConstNumber str -> VNumber str
                  ConstString str -> VString str
        k v s
      Fetch -> \k -> do
        let State{ip} = s
        if ip >= progSize then k Nothing s else do
          let op = chunk !! ip
          k (Just op) s { ip = ip + 1 } -- TODO: fix when not always 1 byte

data State = State { ip :: Int, stack :: Map Int Value, depth :: Int }

state0 :: State
state0 = State { ip = 0, stack = Map.empty, depth = 0 }
