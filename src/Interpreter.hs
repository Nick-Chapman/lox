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

execDecls :: Env -> [Decl] -> Eff ()
execDecls env = \case
  [] -> pure ()
  d1:ds -> do
    execDecl env d1 $ \env ->
      execDecls env ds

execDecl :: Env -> Decl -> (Env -> Eff ()) -> Eff ()
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

execStat :: Env -> Stat -> Eff ()
execStat env = \case
  SExp e -> do
    _ <- evaluate env e
    pure ()
  SPrint e -> do
    v <- evaluate env e
    Runtime.Print (show v)
  SBlock decls -> do
    execDecls env decls
  SIf cond s1 s2 -> do
    v <- evaluate env cond
    if isTruthy v then execStat env s1 else execStat env s2

evaluate :: Env -> Exp -> Eff Value
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
runtimeError Pos{line} mes = Error (printf "%s\n[line %d] in script" mes line)
