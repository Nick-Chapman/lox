module Interpreter (executeTopDecls) where

import Ast (Stat(..),Exp(..),Func(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Data.Map qualified as Map
import Pos (Pos)
import Runtime (Eff(NewRef,ReadRef,WriteRef),Ref)
import Runtime qualified (Eff(..))
import Text.Printf (printf)
import Value (Value(..),Env(..),Closure(..),Method(..),BoundMethod(..),ClassValue(..),InstanceValue(..),vequal,isTruthy)

emptyEnv :: Env
emptyEnv = Env Map.empty

executeTopDecls :: [Stat] -> Eff Env
executeTopDecls decls = do
  rClock <- NewRef VNativeClockFun
  let globals = insertEnv emptyEnv "clock" rClock
  executeDecls globals decls

executeDecls :: Env -> [Stat] -> Eff Env
executeDecls globals = \case
  [] -> pure globals
  d1:ds -> do
    let ret = Nothing
    execStat globals ret globals d1 $ \globals ->
      executeDecls globals ds

execStat :: Env -> Maybe (Value -> Eff a) -> Env -> Stat -> (Env -> Eff a) -> Eff a
execStat globals ret = execute where

  execute env = \case
    SReturn pos expOpt -> \_ignored_k -> do
      v <- case expOpt of Just exp -> evaluate globals env exp; Nothing -> pure VNil
      case ret of
        Just r -> r v
        Nothing -> Runtime.Error pos "Can't return from top-level code." -- prevented by resolved pass
    SExp e -> \k -> do
      _ <- evaluate globals env e
      k env
    SPrint e -> \k -> do
      v <- evaluate globals env e
      Runtime.Print (show v)
      k env
    SBlock stats -> \k -> do
      let
        finish = k env -- restore env as of block entry
        loop env ds = case ds of
          [] -> finish
          d1:ds -> do
            execute env d1 $ \env ->
              loop env ds
      loop env stats
    SIf cond s1 s2 -> \k -> do
      v <- evaluate globals env cond
      if isTruthy v then execute env s1 k else execute env s2 k
    again@(SWhile cond body) -> \k -> do
      v <- evaluate globals env cond
      if not (isTruthy v) then k env else do
        execute env body $ \_env -> execute env again k
    SFor (init,cond,update) body -> \k -> do
      let deSugared = SBlock [ init , SWhile cond $ SBlock [body,update] ]
      execute env deSugared k
    SVarDecl Identifier{name} e -> \k -> do
      r <- evaluate globals env e >>= NewRef
      k (insertEnv env name r)
    SFunDecl function@Func{name=Identifier{name}} -> \k -> do
      r <- NewRef (closeFunction pure env function)
      k (insertEnv env name r)
    SClassDecl Identifier{name=className} methods -> \k -> do
      classIdentity <- NewRef ()
      let methodMap = Map.fromList [ (name,closeMethod className env func)
                                   | func@Func{name=Identifier{name}} <- methods
                                   ]
      classR <- NewRef (VClass ClassValue { classIdentity, className, methodMap })
      k (insertEnv env className classR)

closeFunction :: (Value -> Eff Value) -> Env -> Func -> Value
closeFunction ret env Func{name=Identifier{name=fname},formals,statements} = do
  VFunc $ Closure fname $ \self globals pos args -> do
    checkArity pos (length formals) (length args)
    Runtime.WithFunctionCall pos (fname++"()") $ do
      selfR <- NewRef self
      env <- pure $ insertEnv env fname selfR
      env <- bindArgs env (zip formals args)
      let k _ = ret VNil
      execStat globals (Just ret) env (SBlock statements) k

closeMethod :: String -> Env -> Func -> Method
closeMethod className env Func{name=Identifier{name=fname},formals,statements} = do
  Method $ \this@InstanceValue{myClass} -> do
    thisR <- NewRef (VInstance this)
    classR <- NewRef (VClass myClass)
    env <- pure $ insertEnv env className classR
    env <- pure $ insertEnv env "this" thisR
    identity <- NewRef ()
    -- methods are not directly self recursive; must go via "this"
    pure $ BoundMethod identity $ Closure fname $ \_self globals pos args -> do
      checkArity pos (length formals) (length args)
      Runtime.WithFunctionCall pos (fname++"()") $ do
        env <- bindArgs env (zip formals args)
        let retme _ = pure (VInstance this)
        let ret = if fname == "init" then retme else pure
        let k _ = ret VNil
        execStat globals (Just ret) env (SBlock statements) k

makeInstance :: ClassValue -> Env -> Pos -> [Value] -> Eff Value
makeInstance cv globals pos args = do
  let ClassValue{methodMap} = cv
  fields <- NewRef Map.empty
  let this = InstanceValue {myClass=cv, fields}
  case Map.lookup "init" methodMap of
    Nothing -> do
      checkArity pos 0 (length args)
      pure (VInstance this)
    Just (Method m) -> do
      bm <- m this
      let BoundMethod{closure} = bm
      _ignoredInitRV <- runClosure closure globals pos args
      pure (VInstance this)

bindArgs :: Env -> [(Identifier,Value)] -> Eff (Env)
bindArgs env = \case
  [] -> pure env
  (Identifier{name=x},v):more -> do
    r <- NewRef v
    bindArgs (insertEnv env x r) more

evaluate :: Env -> Env -> Exp -> Eff Value
evaluate globals env = eval where

  eval = \case
    ELit x -> pure (evalLit x)
    EGrouping e -> eval e
    EBinary pos e1 op e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      evalOp2 pos v1 v2 op
    EUnary pos op e -> do
      v <- eval e
      evalOp1 pos op v
    EVar Identifier{pos,name=x} -> do
      r <- lookup pos x
      ReadRef r
    EThis pos -> do
      r <- lookup pos "this"
      ReadRef r
    EAssign Identifier{pos,name=x} e -> do
      r <- lookup pos x
      v <- eval e
      WriteRef r v
      pure v
    ELogicalAnd e1 e2 -> do
      v1 <- eval e1
      if isTruthy v1 then eval e2 else pure v1
    ELogicalOr e1 e2 -> do
      v1 <- eval e1
      if not (isTruthy v1) then eval e2 else pure v1
    ECall pos func args -> do
      vfunc <- eval func
      vargs <- mapM eval args
      asFunction vfunc globals pos vargs

    EGetProp e Identifier{pos,name=x} -> do
      eval e >>= \case
        VInstance this@InstanceValue{fields,myClass=ClassValue{methodMap}} -> do
          m <- ReadRef fields
          case Map.lookup x m of
            Just v -> pure v
            Nothing -> do
              case Map.lookup x methodMap of
                Just (Method m) -> VBoundMethod <$> m this
                Nothing -> do
                  Runtime.Error pos (printf "Undefined property '%s'." $ x)
        _ ->
          Runtime.Error pos "Only instances have properties."

    ESetProp e1 Identifier{pos,name=x} e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      case v1 of
        VInstance InstanceValue{fields} -> do
          m <- ReadRef fields
          WriteRef fields (Map.insert x v2 m)
          pure v2
        _ ->
          Runtime.Error pos "Only instances have fields."

  lookup :: Pos -> String -> Eff (Ref Value)
  lookup pos x =
    case lookupEnv env x of
      Just r -> pure r
      Nothing -> do
        case lookupEnv globals x of
          Just r -> pure r
          Nothing -> do
            Runtime.Error pos (printf "Undefined variable '%s'." $ x)

insertEnv :: Env -> String -> Ref Value -> Env
insertEnv (Env m) x r = Env (Map.insert x r m)

lookupEnv :: Env -> String -> Maybe (Ref Value)
lookupEnv (Env m) x = Map.lookup x m

evalLit :: Lit -> Value
evalLit = \case
  LNil{} -> VNil
  LBool b -> VBool b
  LNumber n -> VNumber n
  LString s -> VString s

evalOp1 :: Pos -> Op1 -> Value -> Eff Value
evalOp1 pos = \case
  Negate -> vnegate pos
  Not -> \v -> pure (VBool (not (isTruthy v)))

evalOp2 :: Pos -> Value -> Value -> Op2 -> Eff Value
evalOp2 pos v1 v2 = \case
  Add -> vadd pos (v1,v2)
  Sub -> VNumber <$> binary pos (-) v1 v2
  Mul -> VNumber <$> binary pos (*) v1 v2
  Div -> VNumber <$> binary pos (/) v1 v2
  Equals -> pure (VBool (vequal v1 v2))
  NotEquals -> pure (VBool (not (vequal v1 v2)))
  Less -> VBool <$> binary pos (<) v1 v2
  LessEqual -> VBool <$> binary pos (<=) v1 v2
  Greater -> VBool <$> binary pos (>) v1 v2
  GreaterEqual -> VBool <$> binary pos (>=) v1 v2
  where
  binary pos f v1 v2 = do
    n1 <- asNumber pos "Operands must be numbers." v1
    n2 <- asNumber pos "Operands must be numbers." v2
    pure (f n1 n2)

asFunction :: Value -> Env -> Pos -> [Value] -> Eff Value
asFunction func globals pos args = case func of
  VFunc closure -> runClosure closure globals pos args
  VClass cv -> makeInstance cv globals pos args
  VBoundMethod BoundMethod{closure} -> runClosure closure globals pos args
  VNativeClockFun -> do
    checkArity pos 0 (length args)
    n <- Runtime.Clock
    pure (VNumber n)
  _ ->
    Runtime.Error pos "Can only call functions and classes."

runClosure :: Closure -> Env -> Pos -> [Value] -> Eff Value
runClosure self@Closure{func} = func (VFunc self) -- tie recursive knot!

checkArity :: Pos -> Int -> Int -> Eff ()
checkArity pos nformals nargs =
  if nformals == nargs then pure () else
    Runtime.Error pos (printf "Expected %d arguments but got %d." nformals nargs)

vnegate :: Pos -> Value -> Eff Value
vnegate pos v1 = do
  n1 <- asNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))

asNumber :: Pos -> String -> Value -> Eff Double
asNumber pos message = \case
  VNumber n -> pure n
  _ -> Runtime.Error pos message

vadd :: Pos -> (Value,Value) -> Eff Value
vadd pos = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> Runtime.Error pos "Operands must be two numbers or two strings."
