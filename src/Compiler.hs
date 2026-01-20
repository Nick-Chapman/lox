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
import VM (Code(..),runCode)

executeTopDecls :: [Stat] -> Eff ()
executeTopDecls decls = do
  case runAsm (compileStats env0 decls) of
    Left (pos,mes) -> Runtime.Error pos mes
    Right code -> do
      --_dumpBC code
      --Print "execute..."
      runCode code

_dumpBC :: Code -> Eff ()
_dumpBC Code{chunk,numbers,strings} = do
  Print "numbers..."
  mapM_ (Print . show) numbers
  Print "strings..."
  mapM_ (Print . show) strings
  Print "ops..."
  mapM_ (Print . show) chunk

data Env = Env { d :: Int, m :: Map String Int } --deriving Show

env0 :: Env
env0 = Env { d = 0, m = Map.empty }

insertEnv :: String -> Env -> Env
insertEnv name Env{d,m} =
  Env {d = d+1, m = Map.insert name d m}

lookupEnv :: Pos -> String -> Env -> Asm Int
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
          i <- EmitConstNum n
          Emit (OP.CONSTANT_NUM i)
        LNil{} -> Emit OP.NIL
        LBool b -> Emit (if b then OP.TRUE else OP.FALSE)
        LString str -> do
          i <- EmitConstString str
          Emit (OP.CONSTANT_STR i)
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
        Emit (OP.GET_LOCAL n)
      EThis{} -> undefined
      EAssign Identifier{pos,name} e -> do
        n <- lookupEnv pos name env
        comp e
        Emit (OP.SET_LOCAL n)
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
  EmitConstNum :: Double -> Asm Int
  EmitConstString :: String -> Asm Int
  Error :: Pos -> String -> Asm a

type Res = Either (Pos,String) Code

runAsm :: Asm () -> Res
runAsm m = loop (State [] [] []) m k0
  where
    k0  () State{ns,ss,ops} = Right $ Code { chunk = reverse ops, numbers = reverse ns, strings = reverse ss }

    loop :: State -> Asm a -> (a -> State -> Res) -> Res
    loop s = \case
      Ret a -> \k -> k a s
      Bind m f -> \k -> loop s m $ \a s -> loop s (f a) k
      Emit op -> \k -> do
        let State{ops} = s
        k () s { ops = op : ops }
      EmitConstNum n -> \k -> do
        let State{ns} = s
        let i = length ns
        k i s { ns = n : ns }
      EmitConstString str -> \k -> do
        let State{ss} = s
        let i = length ss
        k i s { ss = str : ss }
      Error pos mes -> \_ignoredK -> do
        Left (pos,mes)

data State = State { ns :: [Double], ss :: [String], ops :: [Op] }
