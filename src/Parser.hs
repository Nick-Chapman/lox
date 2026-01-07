module Parser (tryParse) where

import Ast (Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Control.Applicative (many,some)
import Data.Text (Text)
import Par4 (parse,Par,char,lit,sat,alts,noError,skip,position,reject)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha,isNumber,isDigit)

tryParse :: Text -> Either String [Stat]
tryParse = Par4.parse start

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

  decl = alts [funDecl, varDecl, stat]

  varDecl = do
    key "var"
    x <- identifier "variable name"
    eopt <- alts
      [ do key "="; expression
      , pure (ELit LNil)
      ]
    key ";"
    pure (SVarDecl x eopt)

  funDecl = do
    key "fun"
    name <- identifier "fun-name"
    xs <- parameters
    body <- blockStat
    pure (SFunDecl name xs body)

  identifier :: String -> Par Identifier
  identifier expect = nibble $ do
    pos <- position
    x <- alpha
    xs <- many (alts [alpha,digit,sat (=='_')])
    let name = x:xs
    if name `notElem` keywords then pure (Identifier { pos, name }) else do
      let message = printf "Error at '%s': Expect %s." name expect
      reject pos message

  stat =
    alts [returnStat, forStat, whileStat, ifStat, printStat, blockStat, expressionStat]

  returnStat = do
    pos <- position
    key "return"
    e <- alts
      [ expression
      , pure (ELit LNil)
      ]
    key ";"
    pure (SReturn pos e)

  forStat = do
    key "for"
    key "("
    init <- alts
      [ varDecl
      , do key ";"; pure (SBlock [])
      , expressionStat
      ]
    cond <- alts
      [ expression
      , pure (ELit (LBool True))
      ]
    key ";"
    update <- alts
      [ do e <- expression; key ")"; pure (SExp e)
      , do key ")"; pure (SBlock [])
      , expectExpression
      ]
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
    alts
      [ do e <- expression; key ";"; pure (SPrint e)
      , expectExpression
      ]

  expectExpression = reject_next "Expect expression."

  expressionStat = do
    e <- expression
    key ";"
    pure (SExp e)

  blockStat = do
    key "{"
    xs <- many decl
    key "}"
    pure (SBlock xs)

  expression = assign

  assign = do
    e1 <- logicalOr
    alts [ do pos <- position; key "="
              case e1 of
                EVar x1 -> do
                  e2 <- assign
                  pure (EAssign x1 e2)
                _ -> reject pos "Error at '=': Invalid assignment target."
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
    key name
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
              op <- alts [ do key "-"; pure Negate
                         , do key "!"; pure Not
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
             , pure e1
             ]

  max :: Int = 255

  parameters :: Par [Identifier]
  parameters = bracketed $ alts [pure [], someArgs max]
    where
      err = reject_next (printf "Can't have more than %d parameters." max)
      someArgs n = if n == 0 then err else do
        e <- identifier "param"
        es <- alts [ pure [], do key ","; someArgs (n-1) ]
        pure (e:es)


  arguments :: Par [Exp]
  arguments = bracketed $ alts [pure [], someArgs max]
    where
      err = reject_next (printf "Can't have more than %d arguments." max)
      someArgs n = if n == 0 then err else do
        e <- expression
        es <- alts [ pure [], do key ","; someArgs (n-1) ]
        pure (e:es)

  primary :: Par Exp
  primary = alts
    [ ELit <$> literal
    , EVar <$> identifier "expression"
    , EGrouping <$> bracketed expression
    ]

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
    key "("
    x <- par
    key ")"
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
    pos <- position
    lit '.'
    reject pos "Error at '.': Expect expression."

  stringLit = nibble $ do
    doubleQuote
    x <- many stringLitChar
    pos <- position
    alts [doubleQuote
         , reject pos "Error: Unterminated string."
         ]
    pure x
    where
      doubleQuote = lit '"'
      stringLitChar = sat $ \c -> c /= '"'


  reject_next :: String -> Par a
  reject_next msg = do
    pos <- position
    c <- char
    reject pos (printf "Error at %s: %s" (show c) msg)

  key s =
    if all isIdentifierChar s && s `notElem` keywords
    then error (printf "Add \"%s\" to keywords list" s)
    else nibble (noError (mapM_ lit s))
    where isIdentifierChar c = Char.isNumber c || Char.isAlpha c || c `elem` "'_"

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

  notNL = do
    _ <- sat (\case '\n' -> False; _ -> True)
    pure ()

  alpha = sat Char.isAlpha
  digit = sat Char.isDigit
