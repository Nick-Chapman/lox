module Disassemble (dis) where

import Code (Code(..))
import OP (Op)
import OP qualified
import Text.Printf (printf)
import Control.Monad (ap,liftM)
import Data.ByteString.Internal (w2c)

dis :: Code -> String
dis code = execDis code (unlines <$> disOps)

disOps :: Dis [String]
disOps = do
  More >>= \case
    False -> pure []
    True -> do
      ip <- FetchIP
      line <- disMaybeDef
      lines <- disOps
      pure (printf "%03d: %s" ip line : lines)

disMaybeDef :: Dis String
disMaybeDef = do
  AtDef >>= \case
    True -> do
      nargs <- fetchArg
      pure (printf "DEF(#args=%d)" nargs)
    False -> do
      op <- FetchOp
      let op0 = pure (show op)
      let op1 = do x <- fetchArg; pure (printf "%s[%d]" (show op) x)
      case op of
        OP.ARG 10 -> do
          pure "-----"
        OP.ARG n -> do
          case isPrintable n of
            True -> do
              ns <- disText [n]
              pure (show ns)
            False ->
              pure (show n)

        OP.NUMBER -> op1
        OP.STRING -> op1
        OP.NIL -> op0
        OP.TRUE -> op0
        OP.FALSE -> op0
        OP.POP -> op0
        OP.INDIRECT -> op0

        OP.GET_LOCAL -> op1
        OP.GET_LOCAL_ind -> op1
        OP.GET_UPVALUE -> op1
        OP.GET_UPVALUE_ind -> op1
        OP.SET_LOCAL -> op1
        OP.SET_LOCAL_ind -> op1
        OP.SET_UPVALUE -> op1
        OP.SET_UPVALUE_ind -> op1

        OP.EQUAL -> op0
        OP.GREATER -> op0
        OP.LESS -> op0
        OP.ADD -> op0
        OP.SUBTRACT -> op0
        OP.MULTIPLY -> op0
        OP.DIVIDE -> op0
        OP.NOT -> op0
        OP.NEGATE -> op0
        OP.PRINT -> op0
        OP.CLOCK -> op0

        OP.JUMP -> do
          loc <- fetchForards
          pure (printf "%s %d" (show op) loc)

        OP.JUMP_IF_FALSE -> do
          loc <- fetchForards
          pure (printf "%s %d" (show op) loc)

        OP.LOOP -> do
          loc <- fetchBackwards
          pure (printf "%s %d" (show op) loc)

        OP.CALL -> do
          _pos <- fetchArg
          nargs <- fetchArg
          pure (printf "%s(#args=%d)" (show op) nargs)

        OP.CLOSURE -> do
          nfree <- fetchArg
          loc <- fetchForards
          MarkDef loc
          pure (printf "%s(#free=%d, loc=%d)" (show op) nfree loc)

        OP.CLOSURE_ind -> do
          nfree <- fetchArg
          loc <- fetchForards
          MarkDef loc
          pure (printf "%s(#free=%d, loc=%d)" (show op) nfree loc)


        OP.RETURN -> op0

disText :: [Int] -> Dis String
disText ns = do
  let done = pure [ w2c (fromIntegral n) | n <- reverse ns ]
  AtDef >>= \case
    True -> done
    False -> do
      op <- FetchOp
      let nope = do PushBack op; done
      case op of
        OP.ARG 0 -> done
        OP.ARG n -> do
          if isPrintable n then disText (n:ns) else nope
        _ -> nope


isPrintable :: Int -> Bool
isPrintable n = n>=32 && n<=126

fetchForards :: Dis Int
fetchForards = do
  hi <- fetchArg
  lo <- fetchArg
  ip <- FetchIP
  pure (ip + 256*hi + lo)

fetchBackwards :: Dis Int
fetchBackwards = do
  hi <- fetchArg
  lo <- fetchArg
  ip <- FetchIP
  pure (ip - (256*hi + lo))

fetchArg :: Dis Int
fetchArg = do
  FetchOp >>= \case
    OP.ARG n -> pure n
    _ -> error "fetchArg"

instance Functor Dis where fmap = liftM
instance Applicative Dis where pure = Ret; (<*>) = ap
instance Monad Dis where (>>=) = Bind

data Dis a where
  Ret :: a -> Dis a
  Bind :: Dis a -> (a -> Dis b) -> Dis b
  More :: Dis Bool
  FetchIP :: Dis Int
  FetchOp :: Dis Op
  MarkDef :: Int -> Dis ()
  AtDef :: Dis Bool
  PushBack :: Op -> Dis ()

execDis :: Code -> Dis a -> a
execDis Code{chunk} m = loop s0 m k0
  where
    s0 = State {ip=0,ops=chunk,defs=[]}
    k0 _ a = a
    loop :: State -> Dis a -> (State -> a -> b) -> b
    loop s dis k = case dis of
      Ret a -> k s a
      Bind m f -> loop s m $ \s a -> loop s (f a) k
      More -> do
        let State{ops} = s
        k s (case ops of [] -> False; _:_ -> True)
      FetchIP -> do
        let State{ip} = s
        k s ip
      FetchOp -> do
        let State{ip,ops} = s
        case ops of
          [] -> error "FetchOp"
          op:ops -> k s { ip = ip+1, ops } op
      MarkDef n -> do
        let State{defs} = s
        k s { defs = n : defs } ()
      AtDef -> do
        let State{ip,defs} = s
        let b = ip `elem` defs
        k s b
      PushBack op -> do
        let State{ip,ops} = s
        k s { ip = ip-1, ops = op : ops } ()

data State = State { ip :: Int, ops :: [Op], defs :: [Int] }
