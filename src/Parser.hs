module Parser (tryParse) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Control.Applicative (many,some)
import Data.Text (Text)
import Data.Text qualified as Text (uncons)
import Par4 (lit,sat,alts,noError,position,reject)
import Par4 qualified (runParser,Par,Config(..))
import Pos (Pos,initPos,tickPos,showPos)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha,isDigit)

type Token = Char
type Par = Par4.Par Pos Token

tryParse :: Text -> Either String [Stat]
tryParse = either (Left . formatError) Right . Par4.runParser Par4.Config
  { start
  , initPos
  , tickPos
  , scanToken = Text.uncons
  }

formatError :: (Pos,Text,Maybe String) -> String
formatError (pos,text,opt) = do
  let tok = case Text.uncons text of Nothing -> "EOF"; Just (x,_) -> show x
  printf "%s Error%s" (showPos pos) $ case opt of
    Just mes -> mes
    Nothing -> printf ": Unexpected %s." tok

start :: Par [Stat]
start = program where

  keywords =
    [ "and"
    , "class"
    , "else"
    , "false"
    , "for"
    , "fun"
    , "if"
    , "nil"
    , "or"
    , "print"
    , "return"
    , "this"
    , "true"
    , "var"
    , "while"
    ]

  program = do
    whitespace
    many decl

  decl = alts [classDecl, funDecl, varDecl, stat]

  classDecl = do
    key "class"
    x <- varName "class name"
    sym "{"
    sym "}"
    pure (SClassDecl x)

  varDecl = do
    key "var"
    x <- varName "variable name"
    eopt <- alts
      [ do sym "="; expression
      , pure (ELit LNil)
      ]
    sym ";"
    pure (SVarDecl x eopt)

  funDecl = do
    key "fun"
    name <- varName "fun-name"
    xs <- parameters
    body <- blockStat
    pure (SFunDecl name xs body)

  stat =
    alts [returnStat, forStat, whileStat, ifStat, printStat, blockStat, expressionStat]

  returnStat = do
    pos <- position
    key "return"
    e <- alts
      [ expression
      , pure (ELit LNil)
      ]
    sym ";"
    pure (SReturn pos e)

  forStat = do
    key "for"
    sym "("
    init <- alts
      [ varDecl
      , do sym ";"; pure (SBlock [])
      , expressionStat
      ]
    cond <- alts
      [ expression
      , pure (ELit (LBool True))
      ]
    sym ";"
    update <- alts
      [ do e <- expression; pure (SExp e)
      , do pure (SBlock [])
      ]
    sym ")"
    body <- stat
    pure (SFor (init,cond,update) body)

  whileStat = do
    key "while"
    cond <- bracketed expression
    body <- stat
    pure (SWhile cond body)

  ifStat = do
    key "if"
    cond <- bracketed expression
    s1 <- stat
    s2 <- alts [ do key "else"; stat, pure (SBlock []) ]
    pure (SIf cond s1 s2)

  printStat = do
    key "print"
    e <- expression
    sym ";"
    pure (SPrint e)

  expressionStat = do
    e <- expression
    sym ";"
    pure (SExp e)

  blockStat = do
    sym "{"
    xs <- many decl
    sym "}"
    pure (SBlock xs)

  expression = assign

  assign = do
    e1 <- logicalOr
    alts [ do sym "="
              case e1 of
                EVar x1 -> do
                  e2 <- assign
                  pure (EAssign x1 e2)
                EGetProp pos e1 x -> do
                  e2 <- assign
                  pure (ESetProp pos e1 x e2)
                _ -> reject " at '=': Invalid assignment target."
         , pure e1
         ]

  logicalOr = leftAssoc logicalAnd $ alts
    [ do key "or"; pure ELogicalOr
    ]

  logicalAnd = leftAssoc equality $ alts
    [ do key "and"; pure ELogicalAnd
    ]

  bin :: String -> Op2 -> Par (Exp -> Exp -> Exp)
  bin name op = do
    pos <- position
    sym name
    pure (\e1 e2 -> EBinary pos e1 op e2)

  equality = leftAssoc comparison $ alts
    [ bin "==" Equals
    , bin "!=" NotEquals
    ]

  comparison = leftAssoc term $ alts
    [ bin "<=" LessEqual
    , bin "<" Less
    , bin ">=" GreaterEqual
    , bin ">" Greater
    ]

  term = leftAssoc factor $ alts
    [ bin "-" Sub
    , bin "+" Add
    ]

  factor = leftAssoc unary $ alts
    [ bin "*" Mul
    , bin "/" Div
    ]

  unary =
    alts [ do pos <- position
              op <- alts [ do sym "-"; pure Negate
                         , do sym "!"; pure Not
                         ]
              e <- unary
              pure (EUnary pos op e)
         , call
         ]

  call :: Par Exp
  call = primary >>= loop
    where
      loop e1 = do
        alts [ do pos <- position
                  xs <- arguments
                  loop (ECall pos e1 xs)
             , do
                 pos <- position
                 sym "."
                 x <- varName "field"
                 loop (EGetProp pos e1 x)
             , pure e1
             ]

  max :: Int = 255

  parameters :: Par [Identifier]
  parameters = bracketed $ alts [pure [], someArgs max]
    where
      err = reject_next (printf "Can't have more than %d parameters." max)
      someArgs n = if n == 0 then err else do
        e <- varName "param"
        es <- alts [ pure [], do sym ","; someArgs (n-1) ]
        pure (e:es)


  arguments :: Par [Exp]
  arguments = bracketed $ alts [pure [], someArgs max]
    where
      err = reject_next (printf "Can't have more than %d arguments." max)
      someArgs n = if n == 0 then err else do
        e <- expression
        es <- alts [ pure [], do sym ","; someArgs (n-1) ]
        pure (e:es)

  reject_next :: String -> Par a
  reject_next msg = do
    c <- next
    reject (printf " at %s: %s" (show c) msg)

  primary :: Par Exp
  primary = alts
    [ ELit <$> literal
    , EVar <$> varName "expression"
    , EGrouping <$> bracketed expression
    ]

  varName :: String -> Par Identifier
  varName expect = nibble $ do
    pos <- position
    name <- identifier
    if name `notElem` keywords then pure (Identifier { pos, name }) else do
      let message = printf " at '%s': Expect %s." name expect
      reject message

  literal :: Par Lit
  literal = alts
    [ do key "nil"; pure LNil
    , do key "true"; pure (LBool True)
    , do key "false"; pure (LBool False)
    , LNumber <$> numberLit
    , LString <$> stringLit
    ]

  leftAssoc :: Par Exp -> Par (Exp -> Exp -> Exp) -> Par Exp
  leftAssoc subPar opPar = subPar >>= loop
    where
      loop e1 = do
        alts [ do f <- opPar
                  e2 <- subPar
                  loop (f e1 e2)
             , pure e1
             ]

  bracketed par = do
    sym "("
    x <- par
    sym ")"
    pure x

  numberLit = alts [nibble (read <$> numberChars), badNumberLit]
    where
      numberChars = do
        before <- some digit
        alts
          [ do lit '.'
               after <- some digit
               pure (before ++ "." ++ after)
          , pure before
          ]

  badNumberLit = do
    lit '.'
    reject " at '.': Expect expression."

  stringLit = nibble $ do
    doubleQuote
    x <- many stringLitChar
    alts [doubleQuote
         , reject ": Unterminated string."
         ]
    pure x
    where
      doubleQuote = lit '"'
      stringLitChar = sat $ \c -> c /= '"'

  sym s = nibble (noError (mapM_ lit s))

  key s =
    if s `notElem` keywords then error (printf "Add \"%s\" to keywords list" s) else do
      nibble $ noError $ do
        s' <- identifier
        if s == s' then pure () else alts []

  identifier :: Par String
  identifier = do
    x <- alpha
    xs <- many (alts [alpha,digit,sat (=='_')])
    pure (x:xs)

  nibble par = do
    x <- par
    whitespace
    pure x

  whitespace = skip (alts [white1, commentToEol])

  white1 = alts [lit ' ', lit '\n', lit '\t']

  commentToEol :: Par ()
  commentToEol = do
    noError (do lit '/'; lit '/')
    skip notNL

  skip p = do _ <- many p; return ()

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()

  alpha = sat Char.isAlpha
  digit = sat Char.isDigit
  next = sat (const True)
