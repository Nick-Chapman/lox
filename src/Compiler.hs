module Compiler (compile) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..),Func(..))
import Code (Code(..))
import Control.Monad (ap,liftM,when)
import Control.Monad.Fix (MonadFix,mfix)
import Data.ByteString.Internal (c2w)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set (Set,(\\),union,singleton)
import Data.Set qualified as Set
import OP (Op)
import OP qualified
import Pos (Pos,toLine)
import Text.Printf (printf)

paramMode :: Mode
paramMode = ModeR -- this is a hack. Goes wrong if a param is assigned AND closed-over

clockMode :: Mode
clockMode = ModeR -- this is a hack. clock might be re-assigned AND closed


compile :: [Stat] -> Either (Pos,String) Code
compile decls = do
  runAsm $ do
    nativeClock emptyEnv $ \globals -> do
      compStats globals decls
      Emit OP.RETURN
      newline

nativeClock :: Env -> (Env -> Asm ()) -> Asm ()
nativeClock env k = mdo
  let arity = 0
  let numFree = 0
  Emit (if (clockMode == ModeL) then OP.CLOSURE else OP.CLOSURE_noind)
  Emit (OP.ARG numFree)
  forwards def
  Emit OP.JUMP; forwards after

  embedFunctionName "<native fn>\0" -- clock is the only native function!
  def <- Here
  Emit (OP.ARG arity)
  Emit OP.CLOCK
  Emit OP.RETURN
  newline

  after <- Here
  k (insertEnv "clock" clockMode env)

compStats :: Env -> [Stat] -> Asm ()
compStats env = \case
  [] -> pure ()
  d1:ds -> compStatThen env d1 ds

compStat :: Env -> Stat -> Asm ()
compStat env stat = compStatThen env stat []

compStatThen :: Env -> Stat -> [Stat]-> Asm ()
compStatThen env = \case

  SPrint e -> \after -> do
    compExp e
    Emit OP.PRINT
    compStats env after

  SExp e -> \after -> do
    compExp e
    Emit OP.POP
    compStats env after

  SBlock stats -> \after -> do
    compStats env stats
    compStats env after

  SIf cond s1 s2 -> \after -> mdo
    compExp cond
    Emit OP.JUMP_IF_FALSE; forwards elseBranch
    -- thenBranch:
    Emit OP.POP
    compStat env s1
    Emit OP.JUMP; forwards rejoin
    elseBranch <- Here
    Emit OP.POP
    compStat env s2
    rejoin <- Here
    compStats env after

  SWhile cond stat -> \after -> mdo
    start <- Here
    compExp cond
    Emit OP.JUMP_IF_FALSE; forwards done
    Emit OP.POP
    compStat env stat
    Emit OP.LOOP
    backwards start
    done <- Here
    Emit OP.POP
    compStats env after

  SFor (init,cond,update) body -> \after -> do
    let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
    compStatThen env deSugared after

  SVarDecl Identifier{pos=_pos,name} e -> \after -> do
    let check1 = isAssigned name after
    let check2 = isClosedOver name after
    let mode = if check1 && check2 then ModeL else ModeR
    compExp e
    when (mode == ModeL) $ Emit OP.INDIRECT
    compStats (insertEnv name mode env) after
    Emit OP.POP

  SReturn _pos expOpt -> \_ignoreK -> do
    case expOpt of
      Nothing -> Emit OP.NIL
      Just exp -> compExp exp
    Emit OP.RETURN

  me@(SFunDecl func@Func{pos,name=Identifier{name=fname},formals,statements}) -> \after -> mdo
    let check1 = isAssigned fname after
    let check2 = isClosedOver fname (me : after)
    let mode = if check1 && check2 then ModeL else ModeR
    Emit (if (mode==ModeL) then OP.CLOSURE else OP.CLOSURE_noind)
    let free = Set.toList $ fvFunc func
    Emit (OP.ARG (length free))
    forwards def
    let env' = insertEnv fname mode env
    sequence_ [ emitCloseVar pos x env' | x <- free ]
    Emit OP.JUMP; forwards afterDef

    newline
    embedFunctionName (printf "<fn %s>\0" fname)
    def <- Here
    let arity = length formals
    Emit (OP.ARG arity)

    let freeWithModes = [ (x,lookupMode x env') | x <- free ]
    let subEnv = foldl (\e name -> insertEnv name paramMode e) (frameEnv freeWithModes)
          [ name | Identifier{name} <- formals ]
    compStats subEnv statements
    Emit OP.NIL
    Emit OP.RETURN
    newline

    afterDef <- Here
    compStats (insertEnv fname mode env) after
    Emit OP.POP

  SClassDecl{} -> do undefined

  where

    emitCloseVar :: Pos -> String -> Env -> Asm ()
    emitCloseVar pos name env' = do
      lookupEnv pos name env' >>= \case
        (VLocal n,_mode) -> do
          Emit (OP.ARG 1)
          Emit (OP.ARG n)
        (VFrame n,_mode) -> do
          Emit (OP.ARG 2)
          Emit (OP.ARG n)

    compExp :: Exp -> Asm ()
    compExp = \case
      EGrouping e -> compExp e

      ELit lit -> case lit of
        LNumber n -> do
          i <- EmitConstNum n
          Emit OP.NUMBER
          Emit (OP.ARG i)
        LNil{} -> Emit OP.NIL
        LBool b -> Emit (if b then OP.TRUE else OP.FALSE)
        LString str -> do
          i <- EmitConstStr str
          Emit OP.STRING
          Emit (OP.ARG i)

      EUnary _pos op e  -> do
        compExp e
        case op of
          Negate -> Emit OP.NEGATE
          Not -> Emit OP.NOT

      EBinary _pos e1 op e2 -> do
        compExp e1
        compExp e2
        case op of
          Add -> Emit OP.ADD
          Sub -> Emit OP.SUBTRACT
          Mul -> Emit OP.MULTIPLY
          Div -> Emit OP.DIVIDE
          Equals -> Emit OP.EQUAL
          NotEquals -> do Emit OP.EQUAL; Emit OP.NOT
          Less -> Emit OP.LESS
          LessEqual -> do Emit OP.GREATER; Emit OP.NOT
          Greater -> Emit OP.GREATER
          GreaterEqual -> do Emit OP.LESS; Emit OP.NOT

      EVar Identifier{pos,name} -> do
        (var,mode) <- lookupEnv pos name env
        compVarAccess var
        compMode mode
        Emit OP.DEREF

      EAssign Identifier{pos,name} e -> do
        compExp e
        (var,mode) <- lookupEnv pos name env
        compVarAccess var
        compMode mode
        Emit OP.ASSIGN

      ELogicalAnd e1 e2 -> mdo
        compExp e1
        Emit OP.JUMP_IF_FALSE; forwards after
        Emit OP.POP
        compExp e2
        after <- Here
        pure ()

      ELogicalOr e1 e2 -> mdo
        compExp e1
        Emit OP.JUMP_IF_FALSE; forwards beforeE2
        Emit OP.JUMP; forwards end
        beforeE2 <- Here
        Emit OP.POP
        compExp e2
        end <- Here
        pure ()

      ECall pos func args -> do
        compExp func
        sequence_ [ do compExp arg
                       when (paramMode == ModeL) $ Emit OP.INDIRECT
                  | arg <- args ]
        Emit OP.CALL
        Emit (OP.ARG (toLine pos))
        Emit (OP.ARG (length args))

      EThis{} -> undefined
      ESuperVar{} -> undefined
      EGetProp{} -> undefined
      ESetProp{} -> undefined


compVarAccess :: Var -> Asm ()
compVarAccess = \case
  VLocal n -> do Emit OP.GET_LOCAL; Emit (OP.ARG n)
  VFrame n -> do Emit OP.GET_UPVALUE; Emit (OP.ARG n)

compMode :: Mode -> Asm ()
compMode = \case
  ModeL -> Emit OP.DEREF
  ModeR -> pure ()

forwards :: Int -> Asm ()
forwards a = mdo
  emitShortRelativeDistance (a - b)
  b <- Here
  pure ()

backwards :: Int -> Asm ()
backwards a = mdo
  emitShortRelativeDistance (- (a - b))
  b <- Here
  pure ()

emitShortRelativeDistance :: Int -> Asm ()
emitShortRelativeDistance dist = do
  let lo = dist `mod` 256
  let hi = dist `div` 256
  Emit (OP.ARG $ check hi)
  Emit (OP.ARG $ check lo)
    where
      check x =
        if dist < 0 then error "emitShortRelativeDistance: negative" else
          if dist > 65535 then error "emitShortRelativeDistance: too big" else
            x

embedFunctionName :: String -> Asm ()
embedFunctionName printName = do
  embedText printName
  Emit (OP.ARG (length printName))

embedText :: String -> Asm ()
embedText str = sequence_ [ Emit (OP.ARG $ fromIntegral $ c2w c) | c <- str ]

newline :: Asm () -- make bytecode nicer for human consumption
newline = embedText "\n"


----------------------------------------------------------------------
-- environment

-- Is a var an L-value (with an extra indirection) or an R-value?
data Mode = ModeL | ModeR deriving Eq

data Var = VLocal Int | VFrame Int deriving Show

data Env = Env { d :: Int, m :: Map String (Var,Mode) }

emptyEnv :: Env
emptyEnv = Env { d = 0, m = Map.empty }

frameEnv :: [(String,Mode)] -> Env
frameEnv xs = do
  Env { d = 1, m = Map.fromList [ (name,(VFrame n,mode)) | (n,(name,mode)) <- zip [0..] xs] }

insertEnv :: String -> Mode -> Env -> Env
insertEnv name mode Env{d,m} =
  Env {d = d+1, m = Map.insert name (VLocal d, mode) m}

lookupEnv :: Pos -> String -> Env -> Asm (Var,Mode)
lookupEnv pos name Env{m} =
  maybe err pure $ Map.lookup name m
  where err = do Error pos (printf "Undefined variable '%s'." $ name); pure (VLocal 255,ModeL)

lookupMode :: String -> Env -> Mode -- for use in closing vars
lookupMode name Env{m} =
  snd $ maybe err id $ Map.lookup name m
  where err = error (show("lookupMode",name))

----------------------------------------------------------------------
-- is-closed-over calculation

isClosedOver :: String -> [Stat] -> Bool
isClosedOver name stats =
  name `Set.member` cloStats stats


cloExp :: Exp -> IdSet
cloExp _ = Set.empty

cloStats :: [Stat] -> IdSet
cloStats = \case
  [] -> Set.empty
  s:ss -> cloStatThen s (cloStats ss)

cloStatThen :: Stat -> IdSet -> IdSet
cloStatThen = \case
  SClassDecl{} -> \k -> undefined k
  SVarDecl Identifier{name} e -> \k -> cloExp e `union` (k \\ singleton name)
  SFunDecl func@Func{name=Identifier{name=fname}} -> \k ->
    cloFunc func `union` (k \\ singleton fname)
  s ->
    \k -> cloStat s `union` k

cloStat :: Stat -> IdSet
cloStat = \case
  SPrint e -> cloExp e
  SExp e -> cloExp e
  SBlock stats -> cloStats stats
  SIf cond s1 s2 -> Set.unions [ cloExp cond, cloStat s1, cloStat s2 ]
  SWhile cond stat -> cloExp cond `union` cloStat stat
  SFor (init,cond,update) body -> do
    let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
    cloStat deSugared
  SReturn _ Nothing ->  Set.empty
  SReturn _ (Just e) -> cloExp e
  SVarDecl{} -> error "cloStat/VarDecl"
  SFunDecl{} -> error "cloStat/FunDecl"
  SClassDecl{} -> error "cloStat/ClcloDecl"

cloFunc :: Func -> IdSet
cloFunc Func{formals,statements} =
  fvStats statements \\ Set.fromList [ name | Identifier{name} <- formals ]

----------------------------------------------------------------------
-- is-assigned calculation

isAssigned :: String -> [Stat] -> Bool
isAssigned name stats =
  name `Set.member` assStats stats


assExp :: Exp -> IdSet
assExp = \case
  EGrouping e -> assExp e
  ELit{} -> Set.empty
  EUnary _pos _op e  -> assExp e
  EBinary _pos e1 _op e2 -> assExp e1 `union` assExp e2
  EVar{} -> Set.empty
  EAssign Identifier{name} e -> Set.singleton name `union` assExp e
  ELogicalAnd e1 e2 -> assExp e1 `union` assExp e2
  ELogicalOr e1 e2 -> assExp e1 `union` assExp e2
  ECall _pos func args -> Set.unions [ fvExp e | e <- func:args ]
  EThis{} -> undefined
  ESuperVar{} -> undefined
  EGetProp{} -> undefined
  ESetProp{} -> undefined

assStats :: [Stat] -> IdSet
assStats = \case
  [] -> Set.empty
  s:ss -> assStatThen s (assStats ss)

assStatThen :: Stat -> IdSet -> IdSet
assStatThen = \case
  SClassDecl{} -> \k -> undefined k
  SVarDecl Identifier{name} e -> \k -> assExp e `union` (k \\ singleton name)
  SFunDecl func@Func{name=Identifier{name=fname}} -> \k ->
    (assFunc func `union` k) \\ singleton fname
  s ->
    \k -> assStat s `union` k

assStat :: Stat -> IdSet
assStat = \case
  SPrint e -> assExp e
  SExp e -> assExp e
  SBlock stats -> assStats stats
  SIf cond s1 s2 -> Set.unions [ assExp cond, assStat s1, assStat s2 ]
  SWhile cond stat -> assExp cond `union` assStat stat
  SFor (init,cond,update) body -> do
    let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
    assStat deSugared
  SReturn _ Nothing ->  Set.empty
  SReturn _ (Just e) -> assExp e
  SVarDecl{} -> error "assStat/VarDecl"
  SFunDecl{} -> error "assStat/FunDecl"
  SClassDecl{} -> error "assStat/ClassDecl"

assFunc :: Func -> IdSet
assFunc Func{formals,statements} =
  assStats statements \\ Set.fromList [ name | Identifier{name} <- formals ]


----------------------------------------------------------------------
-- free-var calculation

type IdSet = Set String

fvExp :: Exp -> IdSet
fvExp = \case
  EGrouping e -> fvExp e
  ELit{} -> Set.empty
  EUnary _pos _op e  -> fvExp e
  EBinary _pos e1 _op e2 -> fvExp e1 `union` fvExp e2
  EVar Identifier{name} -> Set.singleton name
  EAssign Identifier{name} e -> Set.singleton name `union` fvExp e
  ELogicalAnd e1 e2 -> fvExp e1 `union` fvExp e2
  ELogicalOr e1 e2 -> fvExp e1 `union` fvExp e2
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
    (fvFunc func `union` k) \\ singleton fname
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
fvFunc Func{formals,statements} =
  fvStats statements \\ Set.fromList [ name | Identifier{name} <- formals ]

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
  EmitConstNum :: Double -> Asm Int
  EmitConstStr :: String -> Asm Int
  Error :: Pos -> String -> Asm ()
  Here :: Asm Int
  Fix :: (a -> Asm a) -> Asm a

type Res = Either (Pos,String) Code
type Err = (Pos,String)

runAsm :: Asm () -> Res
runAsm m = finish (loop emptyTabN emptyTabS 0 m)
  where
    finish :: ((),TabN,TabS,[Op],[Err]) -> Res
    finish ((),tn,ts,chunk,errs) =
      case errs of
        [] -> Right $ Code { numbers = listTabN tn
                           , strings = listTabS ts
                           , chunk }
        err:_ -> Left err

    loop :: TabN -> TabS -> Int -> Asm a -> (a,TabN,TabS,[Op],[Err])
    loop tn ts q = \case
      Ret a -> (a,tn,ts,[],[])
      Bind m f ->
        case loop tn ts q m of
          (a,tn,ts,ops1,errs1) ->
            case loop tn ts (q + length ops1) (f a) of
              (b,tn,ts,ops2,errs2) ->
                (b,tn,ts,ops1++ops2,errs1++errs2)
      Position _pos' m -> loop tn ts q m
      Emit op -> ((),tn,ts,[op],[])
      EmitConstNum n -> do
        let (tn',i) = insertTabN n tn
        (i,tn',ts,[],[])
      EmitConstStr s -> do
        let (ts',i) = insertTabS s ts
        (i,tn,ts',[],[])
      Error pos mes -> ((),tn,ts,[],[(pos,mes)])
      Here -> (q,tn,ts,[],[])
      Fix f -> do
        let x@(a,_,_,_,_) = loop tn ts q (f a)
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
