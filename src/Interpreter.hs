module Interpreter (execute) where

import Ast (Prog(..),Decl(..),Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Environment (Env,emptyEnv,insertEnv,lookupEnv)
import Par4 (Pos(..))
import Runtime (Eff(Print,Error,NewRef,ReadRef,WriteRef),Ref)
import Text.Printf (printf)
import Value (Value(..),vequal,isTruthy)

execute :: Prog -> Eff ()
execute = \case
  Prog decls -> do
    let stat = SBlock decls
    let env = emptyEnv
    let ret _v = pure ()
    let k = pure ()
    execStat ret env stat k

execDecl :: (Value -> Eff a) -> Env Value -> Decl -> (Env Value -> Eff a) -> Eff a
execDecl ret env = \case
  DVarDecl id e -> \k -> do
    r <- evaluate env e >>= NewRef
    k (insertEnv env id r)
  DFunDecl fname formals body -> \k -> do
    r <- NewRef VNil
    env <- pure $ insertEnv env fname r
    let v = VClosure {fname,formals,body,env}
    WriteRef v r
    k (insertEnv env fname r)
  DStat stat -> \k -> do
    execStat ret env stat (k env)

execStat :: (Value -> Eff a) -> Env Value -> Stat -> Eff a -> Eff a
execStat ret env = \case
  SReturn _pos exp -> \_ignored_k -> do
    v <- evaluate env exp
    ret v
  SExp e -> \k -> do
    _ <- evaluate env e
    k
  SPrint e -> \k -> do
    v <- evaluate env e
    Runtime.Print (show v)
    k
  SBlock decls -> \k -> do
    execDecls ret env decls k
  SIf cond s1 s2 -> \k -> do
    v <- evaluate env cond
    if isTruthy v then execStat ret env s1 k else execStat ret env s2 k
  again@(SWhile cond body) -> \k -> do
    v <- evaluate env cond
    if not (isTruthy v) then k else do
      execStat ret env body (execStat ret env again k)
  SFor (init,cond,update) body -> \k -> do
    let deSugared :: Stat = SBlock
          [ init
          , DStat $ SWhile cond $
            SBlock [ DStat body
                   , DStat update
                   ]
          ]
    execStat ret env deSugared k

execDecls :: (Value -> Eff a) -> Env Value -> [Decl] -> Eff a -> Eff a
execDecls ret env ds k = case ds of
  [] -> k
  d1:ds -> do
    execDecl ret env d1 $ \env ->
      execDecls ret env ds k

evaluate :: Env Value -> Exp -> Eff Value
evaluate env = eval where

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
    EVar x -> do
      r <- lookup x
      ReadRef r
    EAssign x e -> do
      v <- eval e
      r <- lookup x
      WriteRef v r
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
      vapply pos vfunc vargs

  lookup :: Identifier -> Eff (Ref Value)
  lookup x =
    case lookupEnv env x of
      Just r -> pure r
      Nothing -> do
        let Identifier{pos,name} = x
        runtimeError pos (printf "Undefined variable '%s'." name)

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

vapply :: Pos -> Value -> [Value] -> Eff Value
vapply pos func args = case func of
  VClosure{formals,body,env} -> do
    checkArity pos (length formals) (length args)
    env <- bindArgs env (zip formals args)
    let ret = pure
    let k = ret VNil
    execStat ret env body k
  _ ->
    runtimeError pos "Can only call functions and classes."

  where

checkArity :: Pos -> Int -> Int -> Eff ()
checkArity pos formals args =
  if formals == args then pure () else
    runtimeError pos (printf "Expected %d arguments but got %d." formals args)

bindArgs :: Env Value -> [(Identifier,Value)] -> Eff (Env Value)
bindArgs env = \case
  [] -> pure env
  (x,v):more -> do
    r <- NewRef v
    bindArgs (insertEnv env x r) more

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
runtimeError Pos{line,col} mes = do
  let andCol = False
  let colS = if andCol then "." ++ show col else ""
  Error (printf "%s\n[line %d%s] in script" mes line colS)
