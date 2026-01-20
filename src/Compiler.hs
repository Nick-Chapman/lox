module Compiler (executeTopDecls) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Map qualified as Map
import OP (Op)
import OP qualified
import Pos (Pos)
import Runtime (Eff(Print))
import Runtime qualified (Eff(Error))
import Text.Printf (printf)
import VM (Code(..),runCode, Const(..))
import Data.Word (Word8)

executeTopDecls :: [Stat] -> Eff ()
executeTopDecls decls = do
  case runAsm (compileTopLevel decls) of
    Left (pos,mes) -> Runtime.Error pos mes
    Right code -> do
      -- _dumpBC code
      --Print "execute..."
      runCode code

_dumpBC :: Code -> Eff ()
_dumpBC Code{constants,chunk=ops} = do
  Print "constants..."
  mapM_ (Print . show) constants
  Print "ops..."
  mapM_ (Print . show) ops

compileTopLevel :: [Stat] -> Asm ()
compileTopLevel stats = do
  compileStats env0 stats

data Env = Env { d :: Word8, m :: Map String Word8 }

env0 :: Env
env0 = Env { d = 0, m = Map.empty }

insertEnv :: String -> Env -> Env
insertEnv name Env{d,m} =
  Env {d = d+1, m = Map.insert name d m}

lookupEnv :: Pos -> String -> Env -> Asm Word8
lookupEnv pos name Env{m} =
  maybe err pure $ Map.lookup name m
  where err = Error pos (printf "Undefined variable '%s'." $ name)

compileStats :: Env -> [Stat] -> Asm ()
compileStats env = \case
  [] -> pure ()
  d1:ds -> do
    compileStat env d1 $ \env ->
      compileStats env ds

compileStat :: Env -> Stat -> (Env -> Asm ()) -> Asm ()
compileStat env = comp
  where
    comp :: Stat -> (Env -> Asm ()) -> Asm ()
    comp = \case
      SPrint e -> \k -> do
        compileExp env e
        Emit OP.PRINT
        k env
      SExp e -> \k -> do
        compileExp env e
        Emit OP.POP
        k env
      SReturn{} -> do undefined
      SBlock stats -> \k -> do
        compileStats env stats
        k env
      SIf{} -> do undefined
      SWhile{} -> do undefined
      SFor{} -> do undefined
      SVarDecl Identifier{name} e -> \k -> do
        compileExp env e
        k (insertEnv name env)
        Emit OP.POP
      SFunDecl{} -> do undefined
      SClassDecl{} -> do undefined

compileExp :: Env -> Exp -> Asm ()
compileExp env = comp
  where
    comp = \case
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
      EBinary _ e1 op e2 -> do
        comp e1
        comp e2
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
      EGrouping e -> comp e
      EUnary _pos op e  -> do
        comp e
        case op of
          Negate -> Emit OP.NEGATE
          Not -> Emit OP.NOT
      EVar Identifier{pos,name} -> do
        n <- lookupEnv pos name env
        Emit OP.GET_LOCAL
        Emit (OP.ARG n)
      EThis{} -> undefined
      EAssign Identifier{pos,name} e -> do
        n <- lookupEnv pos name env
        comp e
        Emit OP.SET_LOCAL
        Emit (OP.ARG n)
      ELogicalAnd{} -> undefined
      ELogicalOr{} -> undefined
      ECall{} -> undefined
      ESuperVar{} -> undefined
      EGetProp{} -> undefined
      ESetProp{} -> undefined

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Emit :: Op -> Asm ()
  EmitConst :: Const -> Asm Word8
  Error :: Pos -> String -> Asm a

type Res = Either (Pos,String) Code

runAsm :: Asm () -> Res
runAsm m = finish <$> loop 0 0 m
  where
    finish ((),constants,chunk) = Code { constants, chunk }
    loop :: Word8 -> Int -> Asm a -> Either (Pos,String) (a,[Const],[Op])
    loop i q = \case
      Ret a -> Right (a,[],[])
      Bind m f ->
        loop i q m >>= \(a,cs1,ops1) -> do
        loop (i + fromIntegral (length cs1)) (q + length ops1) (f a) >>= \(b,cs2,ops2) -> do
          Right (b,cs1++cs2,ops1++ops2)
      Emit op -> Right ((),[],[op])
      EmitConst c -> Right (i,[c],[])
      Error pos mes -> Left (pos,mes)
