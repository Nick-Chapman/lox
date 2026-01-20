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
runCode Code{constants,chunk=ops} = runVM (execOps ops)

  where
    execOps :: [Op] -> VM ()
    execOps ops = sequence_ [ execOp op | op <- ops ]

    execOp :: Op -> VM ()
    execOp = \case
      OP.CONSTANT i ->
        Push $ case constants !! i of
                 ConstNumber str -> VNumber str
                 ConstString str -> VString str

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

runVM :: VM () -> Eff ()
runVM m = loop state0 m $ \() _ -> pure ()
  where
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

data State = State { stack :: Map Int Value, depth :: Int }

state0 :: State
state0 = State { stack = Map.empty, depth = 0 }
