module Resolver (resolveTop) where

import Control.Monad (ap,liftM)
import Ast (Stat(..),Exp(..),Func(..),Identifier(..))
import Pos (Pos,showPos)
import Text.Printf (printf)
import Data.Set (Set)
import Data.Set qualified as Set

data Scope = ScopeTop | ScopeFunc | ScopeInit deriving (Eq,Show)

data Context = Context { scope :: Scope, withinClass :: Maybe Bool }

type Res = [String]

resolveTop :: [Stat] -> Res
resolveTop xs = runCheck $ sequence_ [ resolveStatContext context x | x <- xs ]
  where context = Context { scope = ScopeTop, withinClass = Nothing }

resolveFunc :: Context -> Func -> Check ()
resolveFunc context Func{name,formals,statements} = do
  Define name
  NestedScope $ do
    sequence_ [ Define x | x <- formals ]
    sequence_ [ resolveStatContext context s | s <- statements ]

resolveStatContext :: Context -> Stat -> Check ()
resolveStatContext context@Context{scope,withinClass} = resolveStat
  where
    resolveStat = \case
      SExp e -> resolveExp e
      SPrint e -> resolveExp e
      SBlock xs -> NestedScope $ sequence_ [ resolveStat x | x <- xs ]
      SIf cond s1 s2 -> do resolveExp cond; resolveStat s1; resolveStat s2
      SWhile cond body -> do resolveExp cond; resolveStat body
      SFor (i,c,u) body -> NestedScope $ do resolveStat i; resolveExp c; resolveStat u; resolveStat body
      SVarDecl x@Identifier{name=_a} e -> do
        nested <- IsNested
        case e of
          EVar Identifier{pos,name=_b}
            | _a==_b && nested -> do
                Invalid pos (printf "'%s': Can't read local variable in its own initializer." _a)
          _ -> do
            Define x
            resolveExp e
      SFunDecl func -> do
        let context' = context { scope = ScopeFunc }
        resolveFunc context' func
      SClassDecl classX optSuper ms -> do
        Define classX
        let hasSuper = case optSuper of Nothing -> False; Just{} -> True
        sequence_ [ do resolveFunc context' func
                  | func@Func{name=Identifier{name}} <- ms
                  , let scope' = if name == "init" then ScopeInit else ScopeFunc
                  , let context' = context { scope = scope', withinClass = Just hasSuper }
                  ]
      SReturn _pos Nothing -> pure ()
      SReturn pos (Just exp) ->
        case scope of
          ScopeTop -> Invalid pos "'return': Can't return from top-level code."
          ScopeInit -> Invalid pos "'return': Can't return a value from an initializer."
          ScopeFunc -> do resolveExp exp

    resolveExp :: Exp -> Check ()
    resolveExp = \case
      ELit _x -> pure ()
      EGrouping e -> do resolveExp e
      EBinary _pos e1 _op e2 -> do resolveExp e1; resolveExp e2
      EUnary _pos _op e -> do resolveExp e
      EVar _x -> do pure ()
      EAssign _x e -> do resolveExp e
      ELogicalAnd e1 e2 -> do resolveExp e1; resolveExp e2
      ELogicalOr e1 e2 -> do resolveExp e1; resolveExp e2
      ECall _pos func args -> do resolveExp func; sequence_ [ resolveExp arg | arg <- args ]
      EGetProp e _x -> do resolveExp e
      ESetProp e1 _x e2 -> do resolveExp e1; resolveExp e2
      ESuperVar pos _x ->
        case withinClass of
          Nothing -> Invalid pos "'super': Can't use 'super' outside of a class."
          Just False -> Invalid pos "'super': Can't use 'super' in a class with no superclass."
          Just True -> pure ()
      EThis pos ->
        case withinClass of
          Nothing -> Invalid pos "'this': Can't use 'this' outside of a class."
          Just{} -> pure ()

data Check a where
  Ret :: a -> Check a
  Bind :: Check a -> (a -> Check b) -> Check b
  Invalid :: Pos -> String -> Check ()
  NestedScope :: Check a -> Check a
  Define :: Identifier -> Check ()
  IsNested :: Check Bool

instance Functor Check where fmap = liftM
instance Applicative Check where pure = Ret; (<*>) = ap
instance Monad Check where (>>=) = Bind

type State = Maybe (Set String)

runCheck :: Check () -> Res
runCheck check = loop Nothing check $ \() _ -> []
  where
    loop :: State -> Check a -> (a -> State -> Res) -> Res
    loop s = \case
      Ret a -> \k -> k a s
      Bind m f -> \k -> loop s m $ \a s -> loop s (f a) k
      Invalid pos mes -> \k -> invalid pos mes : k () s
      NestedScope m -> \k -> loop (Just Set.empty) m $ \a _ -> k a s
      Define Identifier{pos,name=x} -> do
        case s of
          Nothing -> \k -> k () Nothing
          Just xs ->
            case x `Set.member` xs of
              False -> \k -> do k () (Just (Set.insert x xs))
              True -> \k -> do
                let mes = printf "'%s': Already a variable with this name in this scope." x
                invalid pos mes : k () s
      IsNested -> \k -> do
        let nested = case s of Just{} -> True; Nothing -> False
        k nested s

invalid :: Pos -> String -> String
invalid pos mes = printf "%s Error at %s" (showPos pos) mes
