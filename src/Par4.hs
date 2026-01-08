module Par4
  ( runParser
  , Config(..), Par
  , lit,sat,alts,noError,reject,position
  ) where

import Control.Applicative (Alternative,empty,(<|>))
import Control.Monad (ap,liftM)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)

data Config pos a where
  Config ::
    { start :: Par pos a
    , initPos :: pos
    , tickPos :: pos -> Char -> pos
    , showPos :: pos -> String
    } -> Config pos a

alts :: [Par pos a] -> Par pos a
lit :: Char -> Par pos ()
sat :: (Char -> Bool) -> Par pos Char
noError :: Par pos a -> Par pos a
position :: Par pos pos
reject :: pos -> String -> Par pos a

alts = foldl Alt empty
lit x = do _ <- sat (== x); pure ()
sat = Satisfy
noError = NoError
position = Position
reject = Reject

instance Alternative (Par pos) where empty = Fail; (<|>) = Alt
instance Applicative (Par pos) where pure = Ret; (<*>) = ap
instance Functor (Par pos) where fmap = liftM
instance Monad (Par pos) where (>>=) = Bind

data Par pos a where
  Position :: Par pos pos
  Reject :: pos -> String -> Par pos a
  Ret :: a -> Par pos a
  Bind :: Par pos a -> (a -> Par pos b) -> Par pos b
  Fail :: Par pos a
  Satisfy :: (Char -> Bool) -> Par pos Char
  NoError :: Par pos a -> Par pos a
  Alt :: Par pos a -> Par pos a -> Par pos a

data K4 pos a b = K4
  { eps :: a -> Res b
  , succ :: pos -> Text -> a -> Res b
  , fail :: Res b
  , err :: pos -> Msg -> Res b
  }

type Res a = Either String a

data Msg = XMessage String

defaultErrorMsg :: Text -> String
defaultErrorMsg text =
  printf "Error: Unexpected %s." (ppNextChar text)
  where
    ppNextChar :: Text -> String
    ppNextChar t =
      case Text.uncons t of
        Nothing -> "EOF"
        Just (c,_) -> show c

mkFinish :: forall pos a. Config pos a -> Text -> K4 pos a a
mkFinish config@Config{initPos} text0 = do
  K4 { eps = mkYes initPos text0
     , succ = mkYes
     , fail = mkNo initPos (XMessage (defaultErrorMsg text0))
     , err = mkNo
     }
  where
    mkNo :: pos -> Msg -> Res a
    mkNo pos msg = Left $ prefixPos config pos msg

    mkYes :: pos -> Text -> a -> Res a
    mkYes pos text a = do
      if Text.null text then Right a else do
        Left $ prefixPos config pos (XMessage (defaultErrorMsg text))

prefixPos :: Config pos a -> pos -> Msg -> String -- TODO: inline
prefixPos Config{showPos} pos (XMessage msg) = printf "%s %s" (showPos pos) msg

runParser :: forall pos x. Config pos x -> Text -> Either String x
runParser config@Config{start,initPos,tickPos} text0 =
  run initPos text0 start (mkFinish config text0)
  where
    run :: pos -> Text -> Par pos a -> K4 pos a b -> Res b
    run p t par k@K4{eps,succ,fail,err} = case par of

      Position -> eps p

      Reject pos message -> err pos (XMessage message)

      Ret x -> eps x

      Fail -> fail

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
