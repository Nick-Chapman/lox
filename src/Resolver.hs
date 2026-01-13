module Resolver (resolveTop) where

import Ast (Stat(..),Exp(..),Func(..))
import Pos (Pos,showPos)
import Text.Printf (printf)

type Res = Either String ()

data Context = Script | Function | Method

invalid :: Pos -> String -> Res
invalid pos mes = Left $ printf "%s %s" (showPos pos) mes

resolveTop :: [Stat] -> Res
resolveTop xs = sequence_ [ resolveStatContext Script x | x <- xs ]

resolveStatContext :: Context -> Stat -> Res
resolveStatContext context = resolveStat
  where
    resolveStat = \case
      SReturn pos exp ->
        case context of
          Script -> invalid pos "Error at 'return': Can't return from top-level code."
          Function -> do resolveExp exp
          Method -> do resolveExp exp
      SExp e -> resolveExp e
      SPrint e -> resolveExp e
      SBlock xs -> sequence_ [ resolveStat x | x <- xs ]
      SIf cond s1 s2 -> do resolveExp cond; resolveStat s1; resolveStat s2
      SWhile cond body -> do resolveExp cond; resolveStat body
      SFor (i,c,u) body -> do resolveStat i; resolveExp c; resolveStat u; resolveStat body
      SVarDecl _id e -> do resolveExp e
      SFunDecl Func{body} -> do resolveStatContext Function body
      SClassDecl _className ms -> do sequence_ [ resolveStatContext Method body | Func{body} <- ms ]

    resolveExp :: Exp -> Res
    resolveExp = \case
      EThis pos ->
        case context of
          Script -> invalid pos "Error at 'this': Can't use 'this' outside of a class."
          Method -> Right ()
          Function -> Right () -- invalid pos "" -- TODO: but might be in a class

      ELit _x -> Right ()
      EGrouping e -> do resolveExp e
      EBinary _pos e1 _op e2 -> do resolveExp e1; resolveExp e2
      EUnary _pos _op e -> do resolveExp e
      EVar _pos _x -> do Right ()
      EAssign _pos _x e -> do resolveExp e
      ELogicalAnd e1 e2 -> do resolveExp e1; resolveExp e2
      ELogicalOr e1 e2 -> do resolveExp e1; resolveExp e2
      ECall _pos func args -> do resolveExp func; sequence_ [ resolveExp arg | arg <- args ]
      EGetProp _pos e _x -> do resolveExp e
      ESetProp _pos e1 _x e2 -> do resolveExp e1; resolveExp e2
