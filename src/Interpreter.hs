module Interpreter (execute) where

import Ast (Prog(..),Decl(..),Stat(..),Exp(..),Op1(..),Op2(..),Lit(..))
import Environment (Env,env0,insertEnv,assignEnv,readEnv)
import Par4 (Pos(..))
import Runtime (runtimeError)
import Value (Value(..),vequal,isTruthy)

execute :: Prog -> IO ()
execute = \case
  Prog decs ->
    execDecls env0 decs

execDecls :: Env -> [Decl] -> IO ()
execDecls env = \case
  [] -> pure ()
  d1:ds -> do
    execDecl env d1 $ \env ->
      execDecls env ds

execDecl :: Env -> Decl -> (Env -> IO ()) -> IO ()
execDecl env = \case
  DStat stat -> \k -> do
    execStat env stat
    k env
  DVarDecl id eopt -> \k -> do
    v <-
      case eopt of
        Just e -> evaluate env e
        Nothing -> pure VNil
    env' <- insertEnv env id v
    k env'

execStat :: Env -> Stat -> IO ()
execStat env = \case
  SExp e -> do
    _ <- evaluate env e
    pure ()
  SPrint e -> do
    v <- evaluate env e
    print v
  SBlock decls -> do
    execDecls env decls

evaluate :: Env -> Exp -> IO Value
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
      readEnv env x
    EAssign x e -> do
      v <- eval e
      assignEnv env x v
      pure v

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

vnegate :: Pos -> Value -> IO Value
vnegate pos v1 = do
  n1 <- getNumber pos "Operand must be a number." v1
  pure (VNumber (negate n1))

getNumber :: Pos -> String -> Value -> IO Double
getNumber pos message = \case
  VNumber n -> pure n
  _ -> runtimeError pos message

vadd :: Pos -> (Value,Value) -> IO Value
vadd pos = \case
  (VNumber n1, VNumber n2) -> pure (VNumber (n1 + n2))
  (VString s1, VString s2) -> pure (VString (s1 ++ s2))
  _ -> runtimeError pos "Operands must be two numbers or two strings."
