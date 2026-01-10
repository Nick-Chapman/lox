module Interpreter (executeTopDecls) where

import Ast (Stat(..),Exp(..),Func(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Data.Map qualified as Map
import Pos (Pos,showPos)
import Runtime (Eff(Print,Error,NewRef,ReadRef,WriteRef,Clock),Ref)
import Text.Printf (printf)
import Value (Value(..),Env(..),vequal,isTruthy)

emptyEnv :: Env
emptyEnv = Env Map.empty

executeTopDecls :: [Stat] -> Eff Env
executeTopDecls decls = do
  r <- NewRef vClock
  let globals = insertEnv emptyEnv (Identifier "clock") r
  executeDecls globals decls

vClock :: Value
vClock = VFunc "<native fn>" $  \_globals pos args -> do
  checkArity pos 0 (length args)
  VNumber <$> Clock

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
        Nothing -> runtimeError pos "Can't return from top-level code."
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
    SFunDecl function@Func{name=fname} -> \k -> do
      r <- NewRef VNil
      env <- pure $ insertEnv env fname r
      WriteRef r (close env function)
      k (insertEnv env fname r)
    SClassDecl className@Identifier{name} methods -> \k -> do
      classR <- NewRef (error "unreachable")
      let
        makeInstance :: Env -> Pos -> [Value] -> Eff Value
        makeInstance globals pos args = do
          r <- NewRef Map.empty
          let this = VInstance name r
          thisR <- NewRef this
          env <- pure $ insertEnv env className classR
          env <- pure $ insertEnv env Identifier{name="this"} thisR
          let mm = Map.fromList [ (name,close env func)
                                | func@Func{name=Identifier{name}} <- methods
                                ]
          WriteRef r mm
          case Map.lookup "init" mm of
            Nothing -> do
              checkArity pos 0 (length args)
              pure this
            Just init -> do
              _ignoredInitRV <- asFunction init globals pos args
              pure this

      WriteRef classR (VFunc name makeInstance)
      k (insertEnv env className classR)

close :: Env -> Func -> Value
close env Func{name=Identifier{name},formals,body} = VFunc (printf "<fn %s>" name) $ \globals pos args -> do
  checkArity pos (length formals) (length args)
  env <- bindArgs env (zip formals args)
  let ret = Just (\v -> pure v)
  let k _ = pure VNil
  execStat globals ret env body k

bindArgs :: Env -> [(Identifier,Value)] -> Eff (Env)
bindArgs env = \case
  [] -> pure env
  (x,v):more -> do
    r <- NewRef v
    bindArgs (insertEnv env x r) more

evaluate :: Env -> Env -> Exp -> Eff Value
evaluate globals env = eval where

  eval = \case
    --EThis pos -> runtimeError pos "TODO: this"
    EThis pos -> do r <- lookup pos (Identifier {name="this"}); ReadRef r
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

    EGetProp pos e Identifier{name} -> do
      eval e >>= \case
        VInstance _ r -> do
          m <- ReadRef r
          case Map.lookup name m of
            Nothing -> runtimeError pos (printf "Undefined property '%s'." name)
            Just v -> pure v
        _ ->
          runtimeError pos "Only instances have properties."

    ESetProp pos e1 Identifier{name} e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      case v1 of
        VInstance _ r -> do
          m <- ReadRef r
          WriteRef r (Map.insert name v2 m)
          pure v2
        _ ->
          runtimeError pos "Only instances have fields."

  lookup :: Pos -> Identifier -> Eff (Ref Value)
  lookup pos x =
    case lookupEnv env x of
      Just r -> pure r
      Nothing -> do
        case lookupEnv globals x of
          Just r -> pure r
          Nothing -> do
            let Identifier{name} = x
            runtimeError pos (printf "Undefined variable '%s'." name)

insertEnv :: Env -> Identifier -> Ref Value -> Env
insertEnv (Env m) Identifier{name} r = Env (Map.insert name r m)

lookupEnv :: Env -> Identifier -> Maybe (Ref Value)
lookupEnv (Env m) Identifier{name} = Map.lookup name m

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
  VFunc _ f -> f globals pos args
  _ ->
    runtimeError pos "Can only call functions and classes."

checkArity :: Pos -> Int -> Int -> Eff ()
checkArity pos nformals nargs =
  if nformals == nargs then pure () else
    runtimeError pos (printf "Expected %d arguments but got %d." nformals nargs)

vnegate :: Pos -> Value -> Eff Value
vnegate pos v1 = do
  n1 <- asNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))

asNumber :: Pos -> String -> Value -> Eff Double
asNumber pos message = \case
  VNumber n -> pure n
  _ -> runtimeError pos message

vadd :: Pos -> (Value,Value) -> Eff Value
vadd pos = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> runtimeError pos "Operands must be two numbers or two strings."

runtimeError :: Pos -> String -> Eff a
runtimeError pos mes =
  Error (printf "%s\n%s in script" mes (showPos pos))
