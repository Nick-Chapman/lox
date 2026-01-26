module VM (runCode) where

import Code (Code(..))
import Control.Monad (ap,liftM)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import OP (Op)
import OP qualified
import Pos (Pos)
import Runtime (Eff(Print,Error))
import Text.Printf (printf)

runCode :: Code -> Eff ()
runCode code = do
  runVM code fetchDispatchLoop

fetchDispatchLoop :: VM ()
fetchDispatchLoop = do
  Fetch >>= \case
    Nothing -> pure () -- Halt
    Just (pos,op) -> do
      dispatch pos op
      fetchDispatchLoop

dispatch :: Pos -> Op -> VM ()
dispatch pos = \case
  OP.CONSTANT_NUM -> do
    i <- FetchArg
    v <- GetConstNum i
    Push v
  OP.CONSTANT_STR -> do
    i <- FetchArg
    v <- GetConstStr i
    Push v

  OP.NIL -> Push VNil
  OP.TRUE -> Push (VBool True)
  OP.FALSE -> Push (VBool False)
  OP.POP -> do _ <- Pop; pure ()

  OP.GET_LOCAL -> do
    i <- FetchArg
    v <- GetSlot i
    Push v

  OP.SET_LOCAL -> do
    i <- FetchArg
    v <- Peek 1
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
    v <- Effect (vadd pos (v1,v2))
    Push v

  OP.SUBTRACT -> execBinary VNumber (-)
  OP.MULTIPLY -> execBinary VNumber (*)
  OP.DIVIDE -> execBinary VNumber (/)

  OP.NOT -> do
    v <- Pop
    Push (VBool (not (isTruthy v)))

  OP.NEGATE -> do
    v <- Pop
    v' <- Effect (vnegate pos v)
    Push v'

  OP.PRINT -> do
    v <- Pop
    Effect (Runtime.Print (show v))

  OP.JUMP -> do
    i <- FetchArg
    ModIP (+ (fromIntegral i - 128)) -- TODO: share the 128 hack

  OP.JUMP_IF_FALSE -> do
    i <- FetchArg
    v <- Peek 1
    if isTruthy v then pure () else ModIP (+ (fromIntegral i - 128))

  OP.CONSTANT_FUNC -> do
    arity <- FetchArg
    i <- FetchArg
    dest <- (+ (fromIntegral i - 128)) <$> GetIP
    Push  $ VFunc FuncDef{ codePointer = dest, arity, upValues = [] }

  OP.CALL -> do
    nActuals <- FetchArg
    Peek (1+nActuals) >>= \case
      VFunc FuncDef{codePointer=newIP,arity=nFormals,upValues} -> do
        Effect (checkArity nFormals nActuals)
        prevIP <- GetIP
        prevBase <- GetBase
        prevUps <- GetUps
        PushCallFrame CallFrame { prevIP, prevBase, prevUps }
        d <- GetDepth
        SetBase (d - nActuals - 1)
        SetIP newIP
        SetUps upValues
      _ -> do
        Effect (Runtime.Error pos "Can only call functions and classes.")

  OP.RETURN -> do
    res <- Pop
    base <- GetBase
    CallFrame { prevIP, prevBase } <- PopCallFrame
    SetIP prevIP
    SetBase prevBase
    SetDepth base
    Push res

  OP.CLOSURE -> do
    arity <- FetchArg
    i <- FetchArg
    dest <- (+ (fromIntegral i - 128)) <$> GetIP
    n <- FetchArg
    let
      getClosedValue :: VM Value
      getClosedValue = do
        Fetch >>= \case
          Nothing -> undefined
          Just (pos,op) -> do
            dispatch pos op
            Pop
    upValues :: [Value] <- sequence $ replicate (fromIntegral n) getClosedValue
    Push $ VFunc FuncDef{ codePointer = dest, arity, upValues }

  OP.GET_UPVALUE -> do
    i <- FetchArg
    v <- GetUpValue i
    Push v

  OP.SET_UPVALUE -> do
    i <- FetchArg
    v <- Peek 1
    SetUpValue i v

  OP.ARG{} ->
    error "dispatch/OP_ARG"

  where

  checkArity :: Word8 -> Word8 -> Eff ()
  checkArity nformals nargs =
    if nformals == nargs then pure () else
      Runtime.Error pos (printf "Expected %d arguments but got %d." nformals nargs)

  execBinary :: (a -> Value) -> (Double -> Double -> a) -> VM ()
  execBinary mk f = do
    v2 <- Pop
    v1 <- Pop
    n <- Effect (binary pos f v1 v2)
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
  Peek :: Word8 -> VM Value
  GetSlot :: Word8 -> VM Value
  SetSlot :: Word8 -> Value -> VM ()
  GetConstNum :: Word8 -> VM Value
  GetConstStr :: Word8 -> VM Value
  Fetch :: VM (Maybe (Pos,Op))
  FetchArg :: VM Word8
  ModIP :: (Int -> Int) -> VM ()
  GetIP :: VM Int
  SetIP :: Int -> VM ()
  GetDepth :: VM Word8
  SetDepth :: Word8 -> VM ()

  GetBase :: VM Word8
  SetBase :: Word8 -> VM ()
  PushCallFrame :: CallFrame -> VM ()
  PopCallFrame :: VM CallFrame

  GetUps :: VM [Value]
  SetUps :: [Value] -> VM ()
  GetUpValue :: Word8 -> VM Value
  SetUpValue :: Word8 -> Value -> VM ()

runVM :: Code -> VM () -> Eff ()
runVM Code{numbers,strings,chunk} m = loop state0 m kFinal
  where
    progSize = length chunk
    kFinal () _ = pure ()
    loop :: State -> VM a -> (a -> State -> Eff ()) -> Eff ()
    loop s = \case
      Ret a -> \k -> k a s
      Bind m f -> \k -> loop s m $ \a s -> loop s (f a) k
      Effect eff -> \k -> do a <- eff; k a s

      -- Push/Pop/Peek are w.r.t the (sp) depth
      Push v -> \k -> do
        let State{depth,stack} = s
        k () s { depth = 1 + depth, stack = Map.insert depth v stack }
      Pop -> \k -> do
        let State{depth,stack} = s
        let v = maybe (error"Pop") id $ Map.lookup (depth-1) stack
        k v s { depth = depth - 1, stack = Map.delete (depth-1) stack }
      Peek n -> \k -> do
        let State{depth,stack} = s
        let v = maybe (error"Peek") id $ Map.lookup (depth-n) stack
        k v s

      -- Get/Set-Slot are wrt to the base
      GetSlot i -> \k -> do
        let State{stack,base} = s
        let v = maybe (error "GetSlot") id $ Map.lookup (base+i) stack
        k v s
      SetSlot i v -> \k -> do
        let State{stack,base} = s
        k () s { stack = Map.insert (base+i) v stack }

      GetConstNum i -> \k -> do
        if fromIntegral i >= length numbers then error (show ("GetConstNum",i)) else do
          let v = case numbers !! fromIntegral i of
                    n -> VNumber n
          k v s
      GetConstStr i -> \k -> do
        if fromIntegral i >= length strings then error (show ("GetConstStr",i)) else do
          let v = case strings !! fromIntegral i of
                    str -> VString str
          k v s
      Fetch -> \k -> do
        let State{ip} = s
        if ip >= progSize then k Nothing s else do
          let posAndOp = chunk !! ip
          k (Just posAndOp) s { ip = ip + 1 }
      FetchArg -> \k -> do
        let State{ip} = s
        case chunk !! ip of
          (_,OP.ARG i) -> k i s { ip = ip + 1 }
          _ -> error "FetchArg"

      ModIP g -> \k -> do
        let State{ip} = s
        k () s { ip = g ip }
      GetIP -> \k -> do
        let State{ip} = s
        k ip s
      SetIP ip-> \k -> do
        k () s { ip }

      GetDepth -> \k -> do
        let State{depth} = s
        k depth s
      SetDepth depth -> \k -> do
        k () s { depth }

      GetBase -> \k -> do
        let State{base} = s
        k base s
      SetBase base -> \k -> do
        k () s { base }

      PushCallFrame cf -> \k -> do
        let State{callStack} = s
        k () s { callStack = cf : callStack }
      PopCallFrame{} -> \k -> do
        let State{callStack} = s
        case callStack of
          [] -> error "PopCallFrame"
          cf:callStack -> do
            k cf s { callStack }

      GetUps -> \k -> do
        let State{ups} = s
        k ups s
      SetUps ups -> \k -> do
        k () s { ups }

      GetUpValue i -> \k -> do
        let State{ups} = s
        let v = ups !! (fromIntegral i)
        k v s

      SetUpValue i v -> \k -> do
        -- TODO: need up-values be a list of Ref Value
        undefined i v k

data State = State
  { ip :: Int
  , stack :: Map Word8 Value
  , depth :: Word8
  , base :: Word8
  , ups :: [Value]
  , callStack :: [CallFrame]
  }

state0 :: State
state0 = State { ip = 0, stack = Map.empty, depth = 0, base = 0, ups = [], callStack = [] }

data CallFrame = CallFrame { prevIP :: Int, prevBase :: Word8, prevUps :: [Value] }

----------------------------------------------------------------------
-- copy and paste of primitive values and operations; add VCodePointer
data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VFunc FuncDef

data FuncDef = FuncDef  { codePointer :: Int, arity :: Word8, upValues :: [Value] }

instance Show Value where
  show = \case
    VNil -> "nil"
    VBool b -> if b then "true" else "false"
    VNumber n -> do
      let s = printf "%f" n
      if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
    VString s -> s
    VFunc{} -> "func"

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
  _ -> False

asNumber :: Pos -> String -> Value -> Eff Double
asNumber pos message = \case
  VNumber n -> pure n
  _ -> Runtime.Error pos message

binary :: Pos -> (Double -> Double -> a) -> Value -> Value -> Eff a
binary pos f v1 v2 = do
  n1 <- asNumber pos "Operands must be numbers." v1
  n2 <- asNumber pos "Operands must be numbers." v2
  pure (f n1 n2)

vadd :: Pos -> (Value,Value) -> Eff Value
vadd pos = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> Runtime.Error pos "Operands must be two numbers or two strings."

vnegate :: Pos -> Value -> Eff Value
vnegate pos v1 = do
  n1 <- asNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))
