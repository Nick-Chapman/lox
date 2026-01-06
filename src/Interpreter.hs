module Interpreter (execute) where

import Ast (Prog(..),Decl(..),Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Environment (Env,emptyEnv,insertEnv,assignEnv,readEnv)
import Par4 (Pos(..))
import Runtime (Eff(Print,Error))
import Text.Printf (printf)
import Value (Value(..),vequal,isTruthy)

execute :: Prog -> Eff ()
execute = \case
  Prog decs ->
    execDecls emptyEnv decs

execDecls :: Env Value -> [Decl] -> Eff ()
execDecls env decls =
  execStatWithReturn env (SBlock decls) $ \_v -> pure ()

execStatWithReturn :: forall a. Env Value -> Stat -> (Value -> Eff a) -> Eff a
execStatWithReturn env stat r = execStat env stat (r VNil)
  where

    execStat :: Env Value -> Stat -> Eff a -> Eff a
    execStat env stat k = do
     case stat of
      SExp e -> do
        _ <- evaluate env e
        k
      SPrint e -> do
        v <- evaluate env e
        Runtime.Print (show v)
        k
      SBlock decls -> do
        execDecls env decls k

      SIf cond s1 s2 -> do
        v <- evaluate env cond
        if isTruthy v then execStat env s1 k else execStat env s2 k
      again@(SWhile cond body) -> do
        v <- evaluate env cond
        if not (isTruthy v) then k else do
          execStat env body (execStat env again k)
      SFor (init,cond,update) body -> do
        let deSugared :: Stat = SBlock
              [ init
              , DStat $ SWhile cond $
                SBlock [ DStat body
                       , DStat update
                       ]
              ]
        execStat env deSugared k

      SReturn _pos exp -> do
        -- ignoring k
        v <- evaluate env exp
        r v

    execDecls :: Env Value -> [Decl] -> Eff a -> Eff a
    execDecls env ds k = case ds of
      [] -> k
      d1:ds -> do
        execDecl env d1 $ \env ->
          execDecls env ds k

    execDecl :: Env Value -> Decl -> (Env Value -> Eff a) -> Eff a
    execDecl env = \case
      DStat stat -> \k -> do
        execStat env stat (k env)
      DVarDecl id eopt -> \k -> do
        v <-
          case eopt of
            Just e -> evaluate env e
            Nothing -> pure VNil
        env' <- insertEnv env id v
        k env'
      DFunDecl fname formals body -> \k -> do
        let vf = VClosure {fname,formals,body,env}
        env' <- insertEnv env fname vf
        k env'

vapply :: Pos -> Value -> [Value] -> Eff Value
vapply pos func args = case func of
  VClosure{fname,formals,body,env} -> do
    checkArity formals args
    let
      bindArgs :: Env Value -> [(Identifier,Value)] -> Eff (Env Value)
      bindArgs env = \case
        [] -> pure env
        (x,v):more -> do
          env' <- insertEnv env x v
          bindArgs env' more

    env <- insertEnv env fname func
    env <- bindArgs env (zip formals args)
    execStatWithReturn env body $ \v -> pure v
  _ ->
    runtimeError pos "Can only call functions and classes."

  where
    checkArity formals args =
      if length formals == length args then pure () else
        runtimeError pos (printf "Expected %d arguments but got %d." (length formals) (length args))


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
      case readEnv env x of
        Just eff -> eff
        Nothing -> unboundVar x
    EAssign x e -> do
      v <- eval e
      case assignEnv env x v of
        Just eff -> eff
        Nothing -> unboundVar x
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

  unboundVar Identifier{pos,name} =
    runtimeError pos (printf "Undefined variable '%s'." name)

  evalLit = \case
    LNil{} -> VNil
    LBool b -> VBool b
    LNumber n -> VNumber n
    LString s -> VString s

  evalOp1 pos = \case
    Negate -> vnegate pos
    Not -> \v -> pure (VBool (not (isTruthy v)))

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

  binary pos f v1 v2 = do
    n1 <- getNumber pos "Operands must be numbers." v1
    n2 <- getNumber pos "Operands must be numbers." v2
    pure (f n1 n2)

vnegate :: Pos -> Value -> Eff Value
vnegate pos v1 = do
  n1 <- getNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))

getNumber :: Pos -> String -> Value -> Eff Double
getNumber pos message = \case
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
