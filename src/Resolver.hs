module Resolver (resolveTop) where

import Control.Monad (ap,liftM)
import Ast (Stat(..),Exp(..),Func(..),Identifier(..))
import Pos (Pos,showPos)
import Text.Printf (printf)
import Data.Set (Set)
import Data.Set qualified as Set

data Scope = ScopeTop | ScopeFunc | ScopeInit

data Context = Context { scope :: Scope, withinClass :: Bool }

type Res = Either String ()

resolveTop :: [Stat] -> Res
resolveTop xs = runCheck $ sequence_ [ NestedScope $ resolveStatContext context x | x <- xs ]
  where context = Context { scope = ScopeTop, withinClass = False }

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
      SFor (i,c,u) body -> do resolveStat i; resolveExp c; resolveStat u; resolveStat body
      SVarDecl x e -> do Define x; resolveExp e
      SFunDecl func -> do
        let context' = context { scope = ScopeFunc }
        resolveFunc context' func
      SClassDecl classX ms -> do
        Define classX
        sequence_ [ do resolveFunc context' func
                  | func@Func{name=Identifier{name}} <- ms
                  , let scope' = if name == "init" then ScopeInit else ScopeFunc
                  , let context' = context { scope = scope', withinClass = True }
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
      EThis pos ->
        case withinClass of
          False -> Invalid pos "'this': Can't use 'this' outside of a class."
          True -> pure ()

data Check a where
  Ret :: a -> Check a
  Bind :: Check a -> (a -> Check b) -> Check b
  Invalid :: Pos -> String -> Check a
  NestedScope :: Check a -> Check a
  Define :: Identifier -> Check ()

instance Functor Check where fmap = liftM
instance Applicative Check where pure = Ret; (<*>) = ap
instance Monad Check where (>>=) = Bind

runCheck :: Check () -> Res
runCheck check = loop Set.empty check $ \a _ -> Right a
  where
    loop :: Set String -> Check a -> (a -> Set String -> Res) -> Res
    loop xs = \case
      Ret a -> \k -> k a xs
      Bind m f -> \k -> loop xs m $ \a xs -> loop xs (f a) k
      Invalid pos mes -> \_ignoredK -> invalid pos mes
      NestedScope m -> \k -> loop Set.empty m $ \a _ -> k a xs
      Define Identifier{pos,name=x} -> \k -> do
        case x `Set.member` xs of
          False -> k () (Set.insert x xs)
          True -> invalid pos (printf "'%s': Already a variable with this name in this scope." x)

invalid :: Pos -> String -> Res
invalid pos mes = Left $ printf "%s Error at %s" (showPos pos) mes
