module Scanner (Tok(..),scanner) where

import Data.Text (Text)
import Data.Text qualified as Text (uncons)
import Pos (Pos,tickPos)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha,isDigit,isSpace)

data Tok
  = TokIdentifier String
  | TokString String
  | TokUnterminatedString String
  | TokKeyword String
  | TokSym String
  | TokNumber String
  | TokOtherChar Char
  | TokComment String
  | TokWhite String
  deriving Eq

instance Show Tok where
  show = \case
    TokIdentifier s -> s
    TokString s -> printf "\"%s\"" s
    TokUnterminatedString s -> printf "\"%s" s
    TokKeyword s -> s
    TokSym s -> s
    TokNumber s -> s
    TokOtherChar c -> [c]
    TokWhite s -> s
    TokComment s -> "//"++s


scanner :: Pos -> Text -> Maybe (Tok,Pos,Text)
scanner pos text =
  case scanTok (XTP text pos) of
    Nothing -> Nothing
    Just (tok,XTP text pos) -> Just (tok, pos, text)

data TP = XTP Text Pos

uncons :: TP -> Maybe (Char,TP)
uncons (XTP text pos) =
  case Text.uncons text of
    Nothing -> Nothing
    Just (c,text) -> Just (c,XTP text (tickPos pos c))

keywords :: [String]
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
  , "super"
  , "this"
  , "true"
  , "var"
  , "while"
  ]

scanTok :: TP -> Maybe (Tok,TP)
scanTok t0 =
  case uncons t0 of
    Nothing -> Nothing
    Just (c,t1) ->
      case c of
        '{' -> Just (TokSym [c],t1)
        '}' -> Just (TokSym [c],t1)
        '(' -> Just (TokSym [c],t1)
        ')' -> Just (TokSym [c],t1)
        ';' -> Just (TokSym [c],t1)
        '+' -> Just (TokSym [c],t1)
        '-' -> Just (TokSym [c],t1)
        '*' -> Just (TokSym [c],t1)
        ',' -> Just (TokSym [c],t1)
        '.' -> Just (TokSym [c],t1)

        '!' ->
          case uncons t1 of
            Just ('=',t2) -> Just (TokSym "!=",t2)
            _ -> Just (TokSym "!",t1)

        '/' ->
          case uncons t1 of
            Just ('/',t2) -> Just (loopCommentToEOL [] t2)
            _ -> Just (TokSym "/",t1)

        '"' -> Just (loopString [] t1)

        _ | Char.isSpace c    -> Just $ loopWhite [c] t1
        _ | Char.isDigit c    -> Just $ loopNumber [c] t1
        _ | Char.isAlpha c    -> Just $ loopIdentOrKeyword [c] t1
        _ | isSymChar c       -> Just $ loopSym [c] t1

        _ ->
          Just (TokOtherChar c, t1)

  where
    loopCommentToEOL :: [Char] -> TP -> (Tok,TP)
    loopCommentToEOL acc text =
      case uncons text of
        Just (c,text) | c/='\n' -> loopCommentToEOL (c:acc) text
        _ -> (TokComment (reverse acc), text)

    loopWhite :: [Char] -> TP -> (Tok,TP)
    loopWhite acc text =
      case uncons text of
        Just (c,text) | Char.isSpace c -> loopWhite (c:acc) text
        _ -> (TokWhite (reverse acc),text)

    loopNumber :: [Char] -> TP -> (Tok,TP)
    loopNumber acc text0 =
      case uncons text0 of
        Just (c,text) | Char.isDigit c -> loopNumber (c:acc) text
        Just ('.',text) ->
          case uncons text of
            Just (c,text) | Char.isDigit c -> loopAfterPoint (c:'.':acc) text
            _ ->
              (TokNumber (reverse acc),text0)
        _ ->
          (TokNumber (reverse acc),text0)

    loopAfterPoint :: [Char] -> TP -> (Tok,TP)
    loopAfterPoint acc text =
      case uncons text of
        Just (c,text) | Char.isDigit c -> loopAfterPoint (c:acc) text
        _ ->
          (TokNumber (reverse acc),text)

    loopString :: [Char] -> TP -> (Tok,TP)
    loopString acc text =
      case uncons text of
        Just ('"',text) -> (TokString (reverse acc), text)
        Just (c,text) -> loopString (c:acc) text
        Nothing -> (TokUnterminatedString (reverse acc),text)

    loopIdentOrKeyword :: [Char] -> TP -> (Tok,TP)
    loopIdentOrKeyword acc text =
      case uncons text of
        Just (c,text) | isIdent c -> loopIdentOrKeyword (c:acc) text
        _ -> do
          let s = reverse acc
          let tok = if s `elem` keywords then TokKeyword s else TokIdentifier s
          (tok,text)

    isIdent c = Char.isAlpha c || Char.isDigit c || c == '_'

    loopSym :: [Char] -> TP -> (Tok,TP)
    loopSym acc text =
      case uncons text of
        Just (c,text) | isSymChar c -> loopSym (c:acc) text
        _ -> (TokSym (reverse acc),text)

    isSymChar c = c `elem` "=<>"
