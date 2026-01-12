module Interpreter (executeTopDecls) where

import Ast (Stat(..),Exp(..),Func(..),Op1(..),Op2(..),Lit(..),Identifier(Identifier,idString))
import Data.Map qualified as Map
import Pos (Pos)
import Runtime (Eff(NewRef,ReadRef,WriteRef),Ref)
import Runtime qualified (Eff(..))
import Text.Printf (printf)
import Value (Value(..),Env(..),vequal,isTruthy)

emptyEnv :: Env
emptyEnv = Env Map.empty

executeTopDecls :: [Stat] -> Eff Env
executeTopDecls decls = do
  vClock <- makeNativeClock
  r <- NewRef vClock
  let globals = insertEnv emptyEnv (Identifier "clock") r
  executeDecls globals decls

makeNativeClock :: Eff Value
makeNativeClock = do
  identity <- NewRef ()
  pure $ VFunc identity "<native fn>" $  \_globals pos args -> do
    checkArity pos 0 (length args)
    VNumber <$> Runtime.Clock

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
    SReturn pos exp -> \_ignored_k -> do
      v <- evaluate globals env exp
      case ret of
        Just r -> r v
        Nothing -> Runtime.Error pos "Can't return from top-level code."
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
    SVarDecl id e -> \k -> do
      r <- evaluate globals env e >>= NewRef
      k (insertEnv env id r)
    SFunDecl function@Func{name} -> \k -> do
      r <- NewRef (error "unreachable")
      env <- pure $ insertEnv env name r
      identity <- NewRef ()
      let closure = close identity pure env function
      WriteRef r closure
      k (insertEnv env name r)
    SClassDecl className methods -> \k -> do
      classR <- NewRef (error "unreachable")
      let
        makeInstance :: Env -> Pos -> [Value] -> Eff Value
        makeInstance globals pos args = do
          r <- NewRef (error "unreachable")
          let this = VInstance className r
          thisR <- NewRef this
          env <- pure $ insertEnv env className classR
          env <- pure $ insertEnv env (Identifier "this") thisR
          let init = Identifier "init"
          let retme _ = pure this
          let binds =
                [ let closure = VMethod (\identity -> close identity ret env func) in
                  (name,closure)
                | func@Func{name} <- methods
                , let ret = if name==init then retme else pure
                ]
          let mm = Map.fromList binds
          WriteRef r mm
          case Map.lookup init mm of
            Nothing -> do
              checkArity pos 0 (length args)
              pure this
            Just v -> do
              initM <- do
                case v of
                  VMethod m -> do
                    identity <- NewRef ()
                    pure (m identity)
                  v -> pure v
              _ignoredInitRV <- asFunction initM globals pos args
              pure this

      classIdentity <- NewRef ()
      WriteRef classR (VFunc classIdentity (idString className) makeInstance)
      k (insertEnv env className classR)

close :: Ref () -> (Value -> Eff Value) -> Env -> Func -> Value
close identity ret env Func{name=fname,formals,body} = do
  let printName = printf "<fn %s>" $ idString fname
  VFunc identity printName $ \globals pos args -> do
    checkArity pos (length formals) (length args)
    Runtime.WithFunctionCall pos (idString fname++"()") $ do
      env <- bindArgs env (zip formals args)
      let k _ = ret VNil
      execStat globals (Just ret) env body k

bindArgs :: Env -> [(Identifier,Value)] -> Eff (Env)
bindArgs env = \case
  [] -> pure env
  (x,v):more -> do
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
    EVar pos x -> do
      r <- lookup pos x
      ReadRef r
    EThis pos -> do
      r <- lookup pos (Identifier "this")
      ReadRef r
    EAssign pos x e -> do
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

    EGetProp pos e x -> do
      eval e >>= \case
        VInstance _ r -> do
          m <- ReadRef r
          case Map.lookup x m of
            Nothing -> Runtime.Error pos (printf "Undefined property '%s'." $ idString x)
            Just v -> do
              case v of
                VMethod m -> do
                  identity <- NewRef ()
                  pure (m identity)
                _ -> pure v
        _ ->
          Runtime.Error pos "Only instances have properties."

    ESetProp pos e1 x e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      case v1 of
        VInstance _ r -> do
          m <- ReadRef r
          WriteRef r (Map.insert x v2 m)
          pure v2
        _ ->
          Runtime.Error pos "Only instances have fields."

  lookup :: Pos -> Identifier -> Eff (Ref Value)
  lookup pos x =
    case lookupEnv env x of
      Just r -> pure r
      Nothing -> do
        case lookupEnv globals x of
          Just r -> pure r
          Nothing -> do
            Runtime.Error pos (printf "Undefined variable '%s'." $ idString x)

insertEnv :: Env -> Identifier -> Ref Value -> Env
insertEnv (Env m) x r = Env (Map.insert x r m)

lookupEnv :: Env -> Identifier -> Maybe (Ref Value)
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
  VFunc _ _ f -> f globals pos args
  _ ->
    Runtime.Error pos "Can only call functions and classes."

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
