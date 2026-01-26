module Compiler (compile) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..),Func(..))
import Code (Code(..))
import Control.Monad (ap,liftM)
import Control.Monad.Fix (MonadFix,mfix)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Word (Word8)
import OP (Op)
import OP qualified
import Pos (Pos,initPos)
import Text.Printf (printf)
import Data.Set qualified as Set
import Data.Set (Set,(\\),union,singleton)


compile :: [Stat] -> Either (Pos,String) Code
compile decls = runAsm (compStats emptyEnv decls)

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

  SVarDecl x e -> \k -> do
    compExp e
    k (insertEnv x env)
    Emit OP.POP

  SReturn _pos expOpt -> \_ignoreK -> do
    case expOpt of
      Nothing -> Emit OP.NIL
      Just exp -> compExp exp
    Emit OP.RETURN

  SFunDecl func@Func{name=fname,formals,statements} -> \k -> mdo
    let free = Set.toList $ fvFunc func
    Emit OP.CLOSURE
    Emit (OP.ARG (fromIntegral $ length formals))
    relativize def
    Emit (OP.ARG (fromIntegral $ length free))
    sequence_ [ emitCloseVar x | x <- free ]
    jump after
    def <- Here
    let subEnv = foldl (flip insertEnv) (frameEnv free) (fname:formals)
    compStats subEnv statements
    Emit OP.NIL
    Emit OP.RETURN
    after <- Here
    k (insertEnv fname env)

  SClassDecl{} -> do undefined

  where

    emitCloseVar :: String -> Asm ()
    emitCloseVar name = do
      let pos = initPos
      lookupEnv pos name env >>= \case
        VLocal n -> do
          Emit OP.GET_LOCAL
          Emit (OP.ARG n)
        VFrame n -> do
          Emit OP.GET_UPVALUE
          Emit (OP.ARG n)

    compExp :: Exp -> Asm ()
    compExp = \case
      EGrouping e -> compExp e

      ELit lit -> case lit of
        LNumber n -> do
          i <- EmitConstNum n
          Emit OP.CONSTANT_NUM
          Emit (OP.ARG i)
        LNil{} -> Emit OP.NIL
        LBool b -> Emit (if b then OP.TRUE else OP.FALSE)
        LString str -> do
          i <- EmitConstStr str
          Emit OP.CONSTANT_STR
          Emit (OP.ARG i)

      EUnary pos op e  -> do
        compExp e
        Position pos $ case op of
          Negate -> Emit OP.NEGATE
          Not -> Emit OP.NOT

      EBinary pos e1 op e2 -> do
        compExp e1
        compExp e2
        Position pos $ case op of
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
        lookupEnv pos name env >>= \case
          VLocal n -> do
            Emit OP.GET_LOCAL
            Emit (OP.ARG n)
          VFrame n -> do
            Emit OP.GET_UPVALUE
            Emit (OP.ARG n)

      EAssign Identifier{pos,name} e -> do
        compExp e
        lookupEnv pos name env >>= \case
          VLocal n -> do
            Emit OP.SET_LOCAL
            Emit (OP.ARG n)
          VFrame n -> do
            Emit OP.SET_UPVALUE
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

      ECall pos func args -> do
        compExp func
        sequence_ [ compExp arg | arg <- args ]
        Position pos $ Emit OP.CALL
        Emit (OP.ARG (fromIntegral $ length args))

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
    if dist > 127 then error "too far forward" else do
      if dist < -128 then error "too far backward" else do
        let u = dist + 128 -- 0..255
        OP.ARG (fromIntegral u)

----------------------------------------------------------------------
-- environment

data Var = VLocal Word8 | VFrame Word8

data Env = Env { d :: Word8, m :: Map String Var }

emptyEnv :: Env
emptyEnv = Env { d = 0, m = Map.empty }

frameEnv :: [String] -> Env
frameEnv xs = Env { d = 0, m = Map.fromList [ (x,VFrame n) | (n,x) <- zip [0..] xs] }

insertEnv :: Identifier -> Env -> Env
insertEnv Identifier{name} Env{d,m} =
  Env {d = d+1, m = Map.insert name (VLocal d) m}

lookupEnv :: Pos -> String -> Env -> Asm Var
lookupEnv pos name Env{m} =
  maybe err pure $ Map.lookup name m
  where err = do Error pos (printf "Undefined variable '%s'." $ name); pure (VLocal 255)

----------------------------------------------------------------------
-- free-var calculation

type IdSet = Set String

fvExp :: Exp -> IdSet
fvExp = \case
  EGrouping e -> fvExp e
  ELit{} -> Set.empty
  EUnary _pos _op e  -> fvExp e
  EBinary _pos e1 _op e2 ->  fvExp e1 `union` fvExp e2
  EVar Identifier{name} -> Set.singleton name
  EAssign Identifier{name} e -> Set.singleton name `union` fvExp e
  ELogicalAnd e1 e2 -> undefined e1 e2
  ELogicalOr e1 e2 -> undefined e1 e2
  ECall _pos func args -> Set.unions [ fvExp e | e <- func:args ]
  EThis{} -> undefined
  ESuperVar{} -> undefined
  EGetProp{} -> undefined
  ESetProp{} -> undefined

fvStats :: [Stat] -> IdSet
fvStats = \case
  [] -> Set.empty
  s:ss -> fvStatThen s (fvStats ss)

fvStatThen :: Stat -> IdSet -> IdSet
fvStatThen = \case
  SClassDecl{} -> \k -> undefined k
  SVarDecl Identifier{name} e -> \k -> fvExp e `union` (k \\ singleton name)
  SFunDecl func@Func{name=Identifier{name=fname}} -> \k ->
    fvFunc func `union` (k \\ singleton fname)
  s ->
    \k -> fvStat s `union` k

fvStat :: Stat -> IdSet
fvStat = \case
  SPrint e -> fvExp e
  SExp e -> fvExp e
  SBlock stats -> fvStats stats
  SIf cond s1 s2 -> Set.unions [ fvExp cond, fvStat s1, fvStat s2 ]
  SWhile cond stat -> fvExp cond `union` fvStat stat
  SFor (init,cond,update) body -> do
    let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
    fvStat deSugared
  SReturn _ Nothing ->  Set.empty
  SReturn _ (Just e) -> fvExp e
  SVarDecl{} -> error "fvStat/VarDecl"
  SFunDecl{} -> error "fvStat/FunDecl"
  SClassDecl{} -> error "fvStat/ClassDecl"

fvFunc :: Func -> IdSet
fvFunc Func{name=fname,formals,statements} =
  fvStats statements \\ Set.fromList [ name | Identifier{name} <- fname:formals ]

----------------------------------------------------------------------
-- ASM

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind
instance MonadFix Asm where mfix = Fix

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Position :: Pos -> Asm a -> Asm a
  Emit :: Op -> Asm ()
  EmitConstNum :: Double -> Asm Word8
  EmitConstStr :: String -> Asm Word8
  Error :: Pos -> String -> Asm ()
  Here :: Asm Int
  Fix :: (a -> Asm a) -> Asm a

type Res = Either (Pos,String) Code
type Err = (Pos,String)

runAsm :: Asm () -> Res
runAsm m = finish (loop initPos emptyTabN emptyTabS 0 m)
  where
    finish :: ((),TabN,TabS,[(Pos,Op)],[Err]) -> Res
    finish ((),tn,ts,chunk,errs) =
      case errs of
        [] -> Right $ Code { numbers = listTabN tn
                           , strings = listTabS ts
                           , chunk }
        err:_ -> Left err

    loop :: Pos -> TabN -> TabS -> Int -> Asm a -> (a,TabN,TabS,[(Pos,Op)],[Err])
    loop pos tn ts q = \case
      Ret a -> (a,tn,ts,[],[])
      Bind m f ->
        case loop pos tn ts q m of
          (a,tn,ts,ops1,errs1) ->
            case loop pos tn ts (q + length ops1) (f a) of
              (b,tn,ts,ops2,errs2) ->
                (b,tn,ts,ops1++ops2,errs1++errs2)
      Position pos' m -> loop pos' tn ts q m
      Emit op -> ((),tn,ts,[(pos,op)],[])
      EmitConstNum n -> do
        let (tn',i) = insertTabN n tn
        (fromIntegral i,tn',ts,[],[])
      EmitConstStr s -> do
        let (ts',i) = insertTabS s ts
        (fromIntegral i,tn,ts',[],[])
      Error pos mes -> ((),tn,ts,[],[(pos,mes)])
      Here -> (q,tn,ts,[],[])
      Fix f -> do
        let x@(a,_,_,_,_) = loop pos tn ts q (f a)
        x

data TabN = TabN { i :: Int , m :: Map Double Int }

listTabN :: TabN -> [Double]
listTabN TabN{m} =
  map fst $ sortBy (comparing snd) $ Map.toList m

emptyTabN :: TabN
emptyTabN = TabN { i = 0, m = Map.empty }

insertTabN :: Double -> TabN -> (TabN,Int)
insertTabN c constants@TabN{i,m} =
  case Map.lookup c m of
    Just i -> (constants,i)
    Nothing -> (TabN { i = i + 1, m = Map.insert c i m }, i)


data TabS = TabS { i :: Int , m :: Map String Int }

listTabS :: TabS -> [String]
listTabS TabS{m} =
  map fst $ sortBy (comparing snd) $ Map.toList m

emptyTabS :: TabS
emptyTabS = TabS { i = 0, m = Map.empty }

insertTabS :: String -> TabS -> (TabS,Int)
insertTabS c constants@TabS{i,m} =
  case Map.lookup c m of
    Just i -> (constants,i)
    Nothing -> (TabS { i = i + 1, m = Map.insert c i m }, i)
