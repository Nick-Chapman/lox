module Parser (tryParse) where

import Ast (Prog(..),Decl(..),Stat(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Control.Applicative (many,some)
import Par4 (parse,Par,lit,sat,alts,opt,noError,skip,position,reject)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha,isNumber,isDigit)

tryParse :: String -> Either String Prog
tryParse = Par4.parse "Expect expression" start

start :: Par Prog
start = program where

  keywords =
    [ "and"
    , "class"
    , "else"
    , "false"
    , "fun"
    , "if"
    , "nil"
    , "or"
    , "print"
    , "this"
    , "true"
    , "var"
    ]

  program = do
    whitespace
    Prog <$> many decl

  decl = alts [varDecl, statDecl]

  statDecl = DStat <$> stat

  varDecl = do
    key "var"
    x <- identifier "variable name"
    eopt <- opt $ do key "="; expression
    key ";"
    pure (DVarDecl x eopt)

  identifier :: String -> Par Identifier
  identifier expect = nibble $ do
    pos <- position
    x <- alpha
    xs <- many (alts [alpha,digit])
    let name = x:xs
    if name `notElem` keywords then pure (Identifier { pos, name }) else do
      let message = printf " at '%s': Expect %s" name expect
      reject pos message

  stat =
    alts [ifStat, printStat, expressionStat, blockStat]

  ifStat = do
    key "if"
    cond <- bracketed expression
    s1 <- stat
    s2 <- alts [ do key "else"; stat, pure (SBlock []) ]
    pure (SIf cond s1 s2)

  printStat = do
    key "print"
    e <- expression
    key ";"
    pure (SPrint e)

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
                _ -> reject pos " at '=': Invalid assignment target"
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

  call = primary

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

  numberLit = nibble (read <$> numberChars)
    where
      numberChars = do
        before <- some digit
        alts
          [ do lit '.'
               after <- some digit
               pure (before ++ "." ++ after)
          , pure before
          ]

  stringLit = nibble $ do
    doubleQuote
    x <- many stringLitChar
    pos <- position
    alts [doubleQuote
         , reject pos ": Unterminated string"
         ]
    pure x
    where
      doubleQuote = lit '"'
      stringLitChar = sat $ \c -> c /= '"'

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
