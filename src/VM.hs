module VM (runCode) where

import Code (Code(..))
import Control.Monad (ap,liftM)
import Data.ByteString.Internal (w2c)
import Data.List (isSuffixOf)
import OP (Op)
import OP qualified
import Pos (Pos)
import Runtime (Ref,Eff(Clock,Print,Error,NewRef,ReadRef,WriteRef))
import Text.Printf (printf)

runCode :: Code -> Eff ()
runCode code = do
  runVM code (fetchDispatchLoop 1)

fetchDispatchLoop :: Int -> VM ()
fetchDispatchLoop _i = do
  _ip <- GetIP
  Fetch >>= \case
    Nothing -> pure () -- Halt
    Just (pos,op) -> do
      --DebugStack
      --Effect (Runtime.Print (show (_ip,op)))
      dispatch pos op
      fetchDispatchLoop (_i+1)

dispatch :: Pos -> Op -> VM ()
dispatch pos = \case
  OP.NUMBER -> do
    i <- FetchArg
    v <- GetConstNum i
    Push v
  OP.STRING -> do
    i <- FetchArg
    v <- GetConstStr i
    Push v
  OP.NIL -> Push VNil
  OP.TRUE -> Push (VBool True)
  OP.FALSE -> Push (VBool False)

  OP.POP -> do
    _ <- Pop
    pure ()
  OP.GET_LOCAL -> do
    i <- FetchArg
    v <- GetSlot i
    Push v

  OP.SET_LOCAL -> do
    i <- FetchArg
    v <- Peek
    r <- expectIndirection <$> GetSlot i
    Effect (WriteRef r v)
  OP.GET_UPVALUE -> do
    i <- FetchArg
    r <- GetUpValue i
    v <- Effect (ReadRef r)
    Push v
  OP.SET_UPVALUE -> do
    i <- FetchArg
    v <- Peek
    r <- GetUpValue i
    Effect (WriteRef r v)

  OP.INDIRECT -> do
    v <- Pop
    r <- Effect (NewRef v)
    Push (VIndirection r)
  OP.DEREF -> do
    r <- expectIndirection <$> Pop
    v <- Effect (ReadRef r)
    Push v

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
    str <- seeValue v
    Effect (Runtime.Print str)
  OP.JUMP -> do
    i <- FetchArg
    ModIP (+ i)
  OP.JUMP_IF_FALSE -> do
    i <- FetchArg
    v <- Peek
    if isTruthy v then pure () else ModIP (+ i)
  OP.LOOP -> do
    i <- FetchArg
    ModIP (\x -> x - i)

  OP.CALL -> do
    pos <- FetchArg
    nActuals <- FetchArg
    r <- expectIndirection <$> PeekSlot (1+nActuals)
    Effect (ReadRef r) >>= \case
      VFunc FuncDef{codePointer,upValues} -> do
        prevIP <- GetIP
        prevBase <- GetBase
        prevUps <- GetUps
        PushCallFrame CallFrame { prevIP, prevBase, prevUps }
        d <- GetDepth
        SetBase (d - nActuals - 1)
        SetUps upValues
        SetIP codePointer
        nFormals <- FetchArg
        Effect (checkArity nFormals nActuals)
      _v -> do
        Effect (Runtime.Error pos "Can only call functions and classes.")

  OP.CLOSURE -> do
    numUpvalues <- FetchArg
    off <- FetchArg
    dest <- (+ off) <$> GetIP
    let
      getClosedValue :: VM (Ref Value)
      getClosedValue = do
        mode <- FetchArg
        i <- FetchArg
        case mode of
          1 -> expectIndirection <$> GetSlot i
          2 -> GetUpValue i
          _ -> error "getClosedValue/mode"

    upValues <- sequence $ replicate numUpvalues getClosedValue
    Push $ VFunc FuncDef{ codePointer = dest, upValues }

  OP.RETURN -> do
    res <- Pop
    base <- GetBase
    CallFrame { prevIP, prevBase, prevUps } <- PopCallFrame
    SetIP prevIP
    SetBase prevBase
    SetUps prevUps
    SetDepth base
    Push res

  OP.CLOCK -> do
    n <- Effect (Runtime.Clock)
    Push (VNumber n)

  OP.ARG{} ->
    error "dispatch/OP_ARG"

  where

  checkArity :: Int -> Int -> Eff ()
  checkArity nformals nargs =
    if nformals == nargs then pure () else
      Runtime.Error pos (printf "Expected %d arguments but got %d." nformals nargs)

  execBinary :: (a -> Value) -> (Double -> Double -> a) -> VM ()
  execBinary mk f = do
    v2 <- Pop
    v1 <- Pop
    n <- Effect (binary pos f v1 v2)
    Push (mk n)


seeValue :: Value -> VM String
seeValue = \case
  VNil -> pure "nil"
  VBool b -> pure (if b then "true" else "false")
  VNumber n -> pure $ do
    let s = printf "%f" n
    if ".0" `isSuffixOf` s then reverse $ drop 2 $ reverse s else s
  VString s -> pure s
  VFunc FuncDef{codePointer} -> do
    len <- FetchByte (codePointer-1)
    fetchString (codePointer-1-len) (len-1) -- loose '\0'
  VIndirection{} -> pure "<indirection>"

fetchString :: Int -> Int -> VM String
fetchString start len =
  sequence [ (w2c . fromIntegral) <$> FetchByte (start+i) | i <- [0.. len-1] ]


instance Functor VM where fmap = liftM
instance Applicative VM where pure = Ret; (<*>) = ap
instance Monad VM where (>>=) = Bind

data VM a where
  Ret :: a -> VM a
  Bind :: VM a -> (a -> VM b) -> VM b
  Effect :: Eff a -> VM a

  Peek :: VM Value
  Push :: Value -> VM ()
  Pop :: VM Value

  PeekSlot :: Int -> VM Value
  GetSlot :: Int -> VM Value

  GetConstNum :: Int -> VM Value
  GetConstStr :: Int -> VM Value
  Fetch :: VM (Maybe (Pos,Op))
  FetchArg :: VM Int
  FetchByte :: Int -> VM Int

  ModIP :: (Int -> Int) -> VM ()
  GetIP :: VM Int
  SetIP :: Int -> VM ()
  GetDepth :: VM Int
  SetDepth :: Int -> VM ()

  GetBase :: VM Int
  SetBase :: Int -> VM ()
  PushCallFrame :: CallFrame -> VM ()
  PopCallFrame :: VM CallFrame

  GetUps :: VM [Ref Value]
  SetUps :: [Ref Value] -> VM ()

  GetUpValue :: Int -> VM (Ref Value)


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
      Peek -> loop s (do v <- Pop; Push v; return v)

      Push v -> \k -> do
        let State{items} = s
        k () s { items = v : items }

      Pop -> \k -> do
        let State{items} = s
        let lr = items !! 0
        k lr s { items = drop 1 items }

      PeekSlot n -> \k -> do
        let State{items} = s
        let v = items !! (n-1)
        k v s

      -- Get/Set are wrt to the base
      GetSlot arg -> \k -> do
        let State{base,items} = s
        let v = reverse items !! (base + arg)
        k v s

      GetConstNum arg -> \k -> do
        let i = arg
        if i >= length numbers then error (show ("GetConstNum",i)) else do
          let v = case numbers !! i of
                    n -> VNumber n
          k v s
      GetConstStr arg -> \k -> do
        let i = arg
        if i >= length strings then error (show ("GetConstStr",i)) else do
          let v = case strings !! i of
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

      FetchByte p -> \k -> do
        case chunk !! p of
          (_,OP.ARG i) -> k i s
          _ -> error "FetchByte"

      ModIP g -> \k -> do
        let State{ip} = s
        k () s { ip = g ip }
      GetIP -> \k -> do
        let State{ip} = s
        k ip s
      SetIP ip-> \k -> do
        k () s { ip }

      GetDepth -> \k -> do
        let State{items} = s
        let depth = length items
        k depth s
      SetDepth depth -> \k -> do
        let State{items} = s
        let items' = drop (length items - depth) items
        k () s { items = items' }

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
          [] -> pure () -- error "PopCallFrame" -- Halt
          cf:callStack -> do
            k cf s { callStack }

      GetUps -> \k -> do
        let State{ups} = s
        k ups s
      SetUps ups -> \k -> do
        k () s { ups }

      GetUpValue arg -> \k -> do
        let State{ups} = s
        let r = ups !! arg
        k r s

data State = State
  { ip :: Int
  , items :: [Value]
  , base :: Int
  , ups :: [Ref Value]
  , callStack :: [CallFrame]
  }

state0 :: State
state0 = State { ip = 0, items = [], base = 0, ups = [], callStack = [] }

expectIndirection :: Value -> Ref Value
expectIndirection = \case VIndirection r -> r; _ -> error "expectIndirection"

data CallFrame = CallFrame { prevIP :: Int, prevBase :: Int, prevUps :: [Ref Value] }

----------------------------------------------------------------------
-- copy and paste of primitive values and operations; add VCodePointer
data Value
  = VNil
  | VBool Bool
  | VNumber Double
  | VString String
  | VFunc FuncDef
  | VIndirection (Ref Value)

data FuncDef = FuncDef  { codePointer :: Int, upValues :: [Ref Value] }

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
