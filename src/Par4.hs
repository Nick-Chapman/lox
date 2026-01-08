module Par4
  ( runParser
  , Config(..), Par
  , lit,sat,alts,noError,reject,position
  ) where

import Control.Applicative (Alternative,empty,(<|>))
import Control.Monad (ap,liftM)
import Data.Text (Text)
import Text.Printf (printf)

data Config pos tok a where
  Config ::
    { start :: Par pos tok a
    , initPos :: pos
    , tickPos :: pos -> tok -> pos
    , showPos :: pos -> String
    , scanToken :: Text -> Maybe (tok,Text)
    , unexpectedMessage :: Maybe tok -> String
    } -> Config pos tok a

alts :: [Par pos tok a] -> Par pos tok a
lit :: Eq tok => tok -> Par pos tok ()
sat :: (tok -> Bool) -> Par pos tok tok
noError :: Par pos tok a -> Par pos tok a
position :: Par pos tok pos
reject :: pos -> String -> Par pos tok a

instance Alternative (Par pos tok) where empty = Fail; (<|>) = Alt
instance Applicative (Par pos tok) where pure = Ret; (<*>) = ap
instance Functor (Par pos tok) where fmap = liftM
instance Monad (Par pos tok) where (>>=) = Bind

alts = foldl Alt empty
lit x = do _ <- sat (== x); pure ()
sat = Satisfy
noError = NoError
position = Position
reject = Reject

data Par pos tok a where
  Position :: Par pos tok pos
  Reject :: pos -> String -> Par pos tok a
  Ret :: a -> Par pos tok a
  Bind :: Par pos tok a -> (a -> Par pos tok b) -> Par pos tok b
  Fail :: Par pos tok a
  Satisfy :: (tok -> Bool) -> Par pos tok tok
  NoError :: Par pos tok a -> Par pos tok a
  Alt :: Par pos tok a -> Par pos tok a -> Par pos tok a

data K4 pos a b = K4
  { eps :: a -> Res b
  , succ :: pos -> Text -> a -> Res b
  , fail :: Res b
  , err :: pos -> String -> Res b
  }

type Res a = Either String a

runParser :: forall pos tok a. Config pos tok a -> Text -> Either String a
runParser Config{start,initPos,tickPos,showPos,scanToken,unexpectedMessage} text0 =
  run initPos text0 start finish
  where

    defaultErr :: Text -> String
    defaultErr text = unexpectedMessage (fst <$> scanToken text)

    finish =
      K4 { eps = yes initPos text0
         , succ = yes
         , fail = nope initPos (defaultErr text0)
         , err = nope
         }
      where
        nope pos mes = Left $ printf "%s %s" (showPos pos) mes
        yes pos text a =
          case scanToken text of
            Nothing -> Right a
            Just{} -> nope pos (defaultErr text)


    run :: forall a b. pos -> Text -> Par pos tok a -> K4 pos a b -> Res b
    run p t par k@K4{eps,succ,fail,err} = case par of

      Position -> eps p

      Reject pos message -> err pos message

      Ret x -> eps x

      Fail -> fail

      Satisfy pred -> do
        case scanToken t of
          Nothing -> fail
          Just (c,t) -> if pred c then succ (tickPos p c) t c else fail

      NoError par -> do
        run p t par K4 { eps = eps
                       , succ = succ
                       , fail = fail
                       , err = \_p _mes -> fail
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
                                                     err p (defaultErr t)  -- fail->error
                                                 , err
                                                 }
                      , fail
                      , err
                      }
