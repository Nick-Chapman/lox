-- | 4-value Parser Combinators
module Par4 (Par,parse,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,skip,separated,terminated,digit,dot,noError,Pos(..),position,reject) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)
import qualified Data.Char as Char

instance Alternative Par where empty = Fail; (<|>) = Alt
instance Applicative Par where pure = Ret; (<*>) = ap
instance Functor Par where fmap = liftM
instance Monad Par where (>>=) = Bind

skip :: Par () -> Par ()
separated :: Par () -> Par a -> Par [a]
terminated :: Par () -> Par a -> Par [a]
opt :: Par a -> Par (Maybe a)
alts :: [Par a] -> Par a
word :: Par String
key :: String -> Par ()
int :: Par Int
ws1 :: Par ()
ws0 :: Par ()
digit :: Par Int
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
dot :: Par Char
sat :: (Char -> Bool) -> Par Char
char :: Par Char
noError :: Par a -> Par a
position :: Par Pos
reject :: Pos -> String -> Par a

skip p = do _ <- many p; return ()
separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
terminated term p = alts [ pure [], do x <- p; term; xs <- terminated term p; pure (x:xs) ]
opt p = alts [ pure Nothing, fmap Just p ]
alts = foldl Alt empty
word = some $ sat Char.isAlpha
key cs = NoError (mapM_ lit cs)
int = foldl (\acc d -> 10*acc + d) 0 <$> some digit
ws1 = do sp; ws0
ws0 = do _ <- many sp; return ()
digit = (\c -> Char.ord c - ord0) <$> sat Char.isDigit where ord0 = Char.ord '0'
sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()
dot = sat (/= '\n')
sat = Satisfy
char = sat (const True)
noError = NoError
position = Position
reject = Reject

data Par a where
  Position :: Par Pos
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Satisfy :: (Char -> Bool) -> Par Char
  NoError :: Par a -> Par a
  Alt :: Par a -> Par a -> Par a
  Reject :: Pos -> String -> Par a

data K4 a b = K4
  { eps :: a -> Res b
  , succ :: Pos -> Text -> a -> Res b
  , fail :: Res b
  , err :: Pos -> Msg -> Res b
  }

type Res a = Either String a

data Msg = XMessage String

prefixPos :: Pos -> Msg -> String
prefixPos pos (XMessage msg) = do
  let Pos{line,col} = pos
  let andCol = False
  let colS = if andCol then "." ++ show col else ""
  printf "[line %d%s] %s" line colS msg

defaultErrorMsg :: Text -> String
defaultErrorMsg text =
  printf "Unexpected %s." (ppNextChar text)
  where
    ppNextChar :: Text -> String
    ppNextChar t =
      case Text.uncons t of
        Nothing -> "EOF"
        Just (c,_) -> show c

mkFinish :: Text -> K4 a a
mkFinish text0 = do
  K4 { eps = mkYes initPos text0
     , succ = mkYes
     , fail = mkNo initPos (XMessage (defaultErrorMsg text0))
     , err = mkNo
     }
  where
    mkNo :: Pos -> Msg -> Res a
    mkNo pos msg = Left $ prefixPos pos msg

    mkYes :: Pos -> Text -> a -> Res a
    mkYes pos text a = do
      if Text.null text then Right a else do
        Left $ prefixPos pos (XMessage (defaultErrorMsg text))

parse :: Par a -> Text -> Either String a
parse parStart text0 =
  run initPos text0 parStart (mkFinish text0)
  where
    run :: Pos -> Text -> Par a -> K4 a b -> Res b
    run p t par k@K4{eps,succ,fail,err} = case par of

      Position -> eps p

      Ret x -> eps x

      Fail -> fail

      Reject pos message -> do
        let msg = XMessage message
        err pos msg

      Satisfy pred -> do
        case Text.uncons t of
          Nothing -> fail
          Just (c,t) -> if pred c then succ (tickPos p c) t c else fail

      NoError par -> do
        run p t par K4 { eps = eps
                       , succ = succ
                       , fail = fail
                       , err = \_p _msg -> fail
                       }

      Alt par1 par2 -> do
        run p t par1 K4{ eps = \a1 ->
                           run p t par2 K4{ eps = \_ -> eps a1 -- left biased
                                          , succ
                                          , fail = eps a1
                                          , err
                                          }
                       , succ
                       , fail = run p t par2 k
                       , err
                       }

      Bind par f -> do
        run p t par K4{ eps = \a -> run p t (f a) k
                      , succ = \p t a ->
                                 run p t (f a) K4{ eps = \a -> succ p t a -- consume
                                                 , succ
                                                 , fail = do
                                                     err p (XMessage (defaultErrorMsg t))  -- fail->error
                                                 , err
                                                 }
                      , fail
                      , err
                      }

data Pos = Pos { line :: Int, col :: Int } deriving (Eq,Ord)

instance Show Pos where
  show Pos{line,col} = show line ++ "'" ++ show col

initPos :: Pos
initPos = Pos { line = 1, col = 0 }

tickPos :: Pos -> Char -> Pos
tickPos Pos {line,col} = \case
  '\n' -> Pos { line = line + 1, col = 0 }
  _ -> Pos { line, col = col + 1 }
