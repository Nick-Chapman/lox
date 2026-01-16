module Parser (tryParse) where

import Ast (Stat(..),Func(..),Exp(..),Op1(..),Op2(..),Lit(..),Identifier(..))
import Control.Applicative (many)
import Data.Text (Text)
import Par4 (alts,position,satisfy,expect,reject,runParser)
import Par4 qualified (Par,Config(..))
import Pos (Pos,initPos,showPos)
import Scanner (Tok(..),scanner)
import Text.Printf (printf)

type Par = Par4.Par Pos Tok

tryParse :: Text -> Either String [Stat]
tryParse = either (Left . formatError) Right . runParser Par4.Config
  { start
  , initPos
  , scanner
  }

formatError :: (Pos,Text,Maybe String) -> String
formatError (pos,text,opt) = do
  case opt of
    Nothing -> do
      let tok = case scanner pos text of Nothing -> "EOF"; Just (x,_,_) -> show x
      printf "%s Error: Unexpected '%s'." (showPos pos) tok

    -- No "at end"
    Just (mes@"Unterminated string") -> printf "%s Error: %s." (showPos pos) mes

    Just mes -> do
      let tok = case scanner pos text of Nothing -> "end"; Just (x,_,_) -> printf "'%s'" (show x)
      printf "%s Error at %s: %s"(showPos pos) tok mes


start :: Par [Stat]
start = program where

  program = do
    whitespace
    many decl

  decl = alts [classDecl, funDecl, varDecl, stat]

  classDecl = do
    key "class"
    me@Identifier{name=myName} <- identifier
    optSuper <- alts
      [ do pure Nothing
      , Just <$> do sym "<"; superClassOf myName
      ]
    sym "{"
    ms <- many funDef
    sym "}"
    pure (SClassDecl me optSuper ms)

  superClassOf :: String -> Par Identifier
  superClassOf childName = do
    super <- alts
      [ identifier
      , expect "Expect superclass name."
      ]
    let Identifier{name=superName} = super
    if superName /= childName then pure super else
      reject "A class can't inherit from itself."

  varDecl = do
    key "var"
    x <- alts [identifier, expect "Expect variable name."]
    eopt <- alts
      [ do sym "="; expression
      , pure (ELit LNil)
      ]
    sym ";"
    pure (SVarDecl x eopt)

  funDecl = do
    key "fun"
    fun <- funDef
    pure (SFunDecl fun)

  funDef = do
    name <- identifier
    sym "(";
    xs <- parameters
    alts
      [ sym ")"
      , expect "Expect ')' after parameters."
      ]
    alts
      [ do sym "{"
      , expect "Expect '{' before function body."
      ]
    statements <- many decl
    sym "}"
    pure Func{ name, formals = xs, statements }

  stat =
    alts [returnStat, forStat, whileStat, ifStat, printStat, blockStat, expressionStat]

  returnStat = do
    pos <- position
    key "return"
    eOpt <- alts
      [ Just <$> expression
      , pure Nothing
      ]
    sym ";"
    pure (SReturn pos eOpt)

  forStat = do
    key "for"
    sym "("
    init <- alts
      [ varDecl
      , expressionStat
      , do sym ";"; pure (SBlock [])
      ]
    cond <- alts
      [ do e <- expression; sym ";"; pure e
      , do sym ";"; pure (ELit (LBool True))
      ]
    update <- alts
      [ do e <- expression; sym ")"; pure (SExp e)
      , do sym ")"; pure (SBlock [])
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
    alts [ do
             sym "="
             case e1 of
               EVar x1 -> do
                 e2 <- assign
                 pure (EAssign x1 e2)
               EGetProp e1 x -> do
                 e2 <- assign
                 pure (ESetProp e1 x e2)
               _ -> do
                 reject "Invalid assignment target."
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
                  xs <- bracketed arguments
                  loop (ECall pos e1 xs)
             , do
                 sym "."
                 alts
                   [ do x <- identifier; loop (EGetProp e1 x)
                   , expect "Expect property name after '.'."
                   ]
             , pure e1
             ]

  max :: Int = 255

  parameters :: Par [Identifier]
  parameters = alts [pure [], someArgs max]
    where
      err = expect (printf "Can't have more than %d parameters." max)
      someArgs n = if n == 0 then err else do
        x <- identifier
        xs <- alts [ pure [], do sym ","; someArgs (n-1) ]
        pure (x:xs)

  arguments :: Par [Exp]
  arguments = alts [pure [], someArgs max]
    where
      err = expect (printf "Can't have more than %d arguments." max)
      someArgs n = if n == 0 then err else do
        e <- expression
        es <- alts [ pure [], do sym ","; someArgs (n-1) ]
        pure (e:es)

  primary :: Par Exp
  primary = alts
    [ grouping
    , literal
    , varRef
    , thisRef
    , superRef
    , unterminated_string
    , badNumberLit
    , expect "Expect expression."
    ]

  grouping =
    EGrouping <$> bracketed expression

  literal = ELit <$> alts
    [ do key "nil"; pure LNil
    , do key "true"; pure (LBool True)
    , do key "false"; pure (LBool False)
    , LNumber <$> numberLit
    , LString <$> stringLit
    ]

  varRef =
    EVar <$> identifier

  thisRef = do
    pos <- position
    key "this"
    pure (EThis pos)

  superRef = do
    pos <- position
    key "super"
    alts
      [ do sym "."; alts
                    [ do x <- identifier; pure (ESuperVar pos x)
                    , expect "Expect superclass method name."
                    ]
      , expect "Expect '.' after 'super'."
      ]

  badNumberLit = do
    sym "."
    reject "Expect expression."

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

  identifier :: Par Identifier
  identifier = do
    pos <- position
    name <- identifierName
    pure (Identifier { pos, name} )

  unterminated_string = do
    satisfy $ \case TokUnterminatedString{} -> Just (); _ -> Nothing
    reject "Unterminated string"

  numberLit :: Par Double
  numberLit = nibble $ satisfy $ \case TokNumber s -> Just (read s); _ -> Nothing

  stringLit :: Par String
  stringLit = nibble $ satisfy $ \case TokString s -> Just s; _ -> Nothing

  sym,key :: String -> Par ()

  sym want = nibble $ satisfy $ \case TokSym got | got==want -> Just (); _ -> Nothing
  key want = nibble $ satisfy $ \case TokKeyword got | got==want -> Just (); _ -> Nothing

  identifierName = nibble $ satisfy $ \case TokIdentifier s -> Just s; _ -> Nothing

  nibble :: Par a -> Par a
  nibble par = do
    x <- par
    whitespace
    pure x

  whitespace :: Par ()
  whitespace = skip (alts [white, commentToEol])
    where
      skip :: Par () -> Par ()
      skip p = do _ <- many p; return ()

      commentToEol = satisfy $ \case TokComment{} -> Just (); _ -> Nothing
      white = satisfy $ \case TokWhite{} -> Just (); _ -> Nothing
