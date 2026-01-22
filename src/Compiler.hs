module Compiler (compile) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Code (Code(..),Const(..))
import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import OP (Op)
import OP qualified
import Pos (Pos)
import Text.Printf (printf)

import Data.List (sortBy)
import Data.Ord (comparing)

compile :: [Stat] -> Either (Pos,String) Code
compile decls = runAsm (compStats env0 decls)

data Env = Env { d :: Word8, m :: Map String Word8 }

env0 :: Env
env0 = Env { d = 0, m = Map.empty }

insertEnv :: String -> Env -> Env
insertEnv name Env{d,m} =
  Env {d = d+1, m = Map.insert name d m}

lookupEnv :: Pos -> String -> Env -> Asm Word8
lookupEnv pos name Env{m} =
  maybe err pure $ Map.lookup name m
  where err = do Error pos (printf "Undefined variable '%s'." $ name); pure 0

compStats :: Env -> [Stat] -> Asm ()
compStats env = \case
  [] -> pure ()
  d1:ds -> do
    compStatThen env d1 $ \env ->
      compStats env ds

compStat :: Env -> Stat -> Asm ()
compStat env stat =
  compStatThen env stat $ \_ -> pure ()

compStatThen :: Env -> Stat -> (Env -> Asm ()) -> Asm ()
compStatThen env = \case

  SPrint e -> \k -> do
    compExp e
    Emit OP.PRINT
    k env

  SExp e -> \k -> do
    compExp e
    Emit OP.POP
    k env

  SBlock stats -> \k -> do
    compStats env stats
    k env

  SIf cond s1 s2 -> \k -> mdo
    compExp cond
    jumpIfFalse afterThen
    compStat env s1
    jump afterElse
    afterThen <- Here
    compStat env s2
    afterElse <- Here
    Emit OP.POP
    k env

  SWhile cond stat -> \k -> mdo
    start <- Here
    compExp cond
    jumpIfFalse done
    Emit OP.POP
    compStat env stat
    jump start
    done <- Here
    Emit OP.POP
    k env

  SFor (init,cond,update) body -> \k -> do
    let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
    compStatThen env deSugared k

  SVarDecl Identifier{name} e -> \k -> do
    compExp e
    k (insertEnv name env)
    Emit OP.POP

  SReturn{} -> do undefined
  SFunDecl{} -> do undefined
  SClassDecl{} -> do undefined

  where

    compExp :: Exp -> Asm ()
    compExp = \case
      EGrouping e -> compExp e

      ELit lit -> case lit of
        LNumber n -> do
          i <- EmitConst (ConstNumber n)
          Emit OP.CONSTANT
          Emit (OP.ARG i)
        LNil{} -> Emit OP.NIL
        LBool b -> Emit (if b then OP.TRUE else OP.FALSE)
        LString str -> do
          i <- EmitConst (ConstString str)
          Emit OP.CONSTANT
          Emit (OP.ARG i)

      EUnary _pos op e  -> do
        compExp e
        case op of
          Negate -> Emit OP.NEGATE
          Not -> Emit OP.NOT

      EBinary _ e1 op e2 -> do
        compExp e1
        compExp e2
        case op of
          Add -> Emit OP.ADD
          Sub -> Emit OP.SUBTRACT
          Mul -> Emit OP.MULTIPLY
          Div{} -> Emit OP.DIVIDE
          Equals{} -> Emit OP.EQUAL
          NotEquals{} -> do Emit OP.EQUAL; Emit OP.NOT
          Less{} -> Emit OP.LESS
          LessEqual{} -> do Emit OP.GREATER; Emit OP.NOT
          Greater{} -> Emit OP.GREATER
          GreaterEqual{} -> do Emit OP.LESS; Emit OP.NOT

      EVar Identifier{pos,name} -> do
        n <- lookupEnv pos name env
        Emit OP.GET_LOCAL
        Emit (OP.ARG n)

      EAssign Identifier{pos,name} e -> do
        n <- lookupEnv pos name env
        compExp e
        Emit OP.SET_LOCAL
        Emit (OP.ARG n)

      ELogicalAnd e1 e2 -> mdo
        compExp e1
        jumpIfFalse after
        Emit OP.POP
        compExp e2
        after <- Here
        pure ()

      ELogicalOr e1 e2 -> mdo
        compExp e1
        jumpIfFalse beforeE2
        jump end
        beforeE2 <- Here
        Emit OP.POP
        compExp e2
        end <- Here
        pure ()

      ECall{} -> undefined

      EThis{} -> undefined
      ESuperVar{} -> undefined
      EGetProp{} -> undefined
      ESetProp{} -> undefined


jump :: Int -> Asm ()
jump i = do Emit OP.JUMP; relativize i

jumpIfFalse :: Int -> Asm ()
jumpIfFalse i = do Emit OP.JUMP_IF_FALSE; relativize i

relativize :: Int -> Asm ()
relativize a = do
  b <- Here
  let dist = a - b - 1
  Emit $
    if dist > 127 then error "jump too far forward" else do
      if dist < -128 then error "jump too far backward" else do
        let u = dist + 128 -- 0..255
        OP.ARG (fromIntegral u)

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind
instance MonadFix Asm where mfix = Fix

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Emit :: Op -> Asm ()
  EmitConst :: Const -> Asm Word8
  Error :: Pos -> String -> Asm ()
  Here :: Asm Int
  Fix :: (a -> Asm a) -> Asm a

type Res = Either (Pos,String) Code
type Err = (Pos,String)

runAsm :: Asm () -> Res
runAsm m = finish (loop constants0 0 m)
  where
    finish :: ((),Constants,[Op],[Err]) -> Res
    finish ((),constants,chunk,errs) =
      case errs of
        [] -> Right $ Code { constants = listConstants constants, chunk }
        err:_ -> Left err

    -- accumulate constants so can share
    loop :: Constants -> Int -> Asm a -> (a,Constants,[Op],[Err])
    loop cs q = \case
      Ret a -> (a,cs,[],[])
      Bind m f ->
        case loop cs q m of
          (a,cs1,ops1,errs1) ->
            case loop cs1 (q + length ops1) (f a) of
              (b,cs2,ops2,errs2) ->
                (b,cs2,ops1++ops2,errs1++errs2)
      Emit op -> ((),cs,[op],[])
      EmitConst c -> do
        let (cs',i) = addConstant c cs
        (fromIntegral i,cs',[],[])
      Error pos mes -> ((),cs,[],[(pos,mes)])
      Here -> (q,cs,[],[])
      Fix f -> do
        let x@(a,_,_,_) = loop cs q (f a)
        x

data Constants = Constants
  { i :: Int
  , m :: Map Const Int
  }

listConstants :: Constants -> [Const]
listConstants Constants{m} =
  map fst $ sortBy (comparing snd) $ Map.toList m

constants0 :: Constants
constants0 = Constants { i = 0, m = Map.empty }

addConstant :: Const -> Constants -> (Constants,Int)
addConstant c constants@Constants{i,m} =
  case Map.lookup c m of
    Just i -> (constants,i)
    Nothing -> (Constants { i = i + 1, m = Map.insert c i m }, i)
