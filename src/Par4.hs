module Par4
  ( runParser
  , Config(..), Par
  , satisfy,alts,expect,reject,noError,position
  ) where

import Control.Applicative (Alternative,empty,(<|>))
import Control.Monad (ap,liftM)
import Data.Text (Text)

data Config pos tok a where
  Config ::
    { start :: Par pos tok a
    , initPos :: pos
    , tickPos :: pos -> tok -> pos
    , scanTok :: Text -> Maybe (tok,Text)
    } -> Config pos tok a

alts :: [Par pos tok a] -> Par pos tok a
satisfy :: (tok -> Maybe a) -> Par pos tok a
noError :: Par pos tok a -> Par pos tok a
position :: Par pos tok pos
expect :: String -> Par pos tok a
reject :: String -> Par pos tok a

instance Alternative (Par pos tok) where empty = Fail; (<|>) = Alt
instance Applicative (Par pos tok) where pure = Ret; (<*>) = ap
instance Functor (Par pos tok) where fmap = liftM
instance Monad (Par pos tok) where (>>=) = Bind

alts = foldl Alt empty
satisfy = Satisfy
noError = NoError
position = Position
expect = Expect
reject = Reject

data Par pos tok a where
  Position :: Par pos tok pos
  Ret :: a -> Par pos tok a
  Bind :: Par pos tok a -> (a -> Par pos tok b) -> Par pos tok b
  Fail :: Par pos tok a
  Expect :: String -> Par pos tok a
  Reject :: String -> Par pos tok a
  Satisfy :: (tok -> Maybe a) -> Par pos tok a
  NoError :: Par pos tok a -> Par pos tok a
  Alt :: Par pos tok a -> Par pos tok a -> Par pos tok a

data K4 pos a b = K4
  { eps :: a -> Res pos b
  , succ :: pos -> Text -> a -> Res pos b
  , fail :: Maybe String -> Res pos b
  , rej :: Maybe String -> Res pos b
  , err :: pos -> Text -> Maybe String -> Res pos b
  }

type Res pos a = Either (pos,Text,Maybe String) a

runParser :: forall pos tok a. Config pos tok a -> Text -> Res pos a
runParser Config{start,initPos,tickPos,scanTok} text0 =
  run initPos text0 start finish
  where

    finish =
      K4 { eps = yes initPos text0
         , succ = yes
         , fail = \mo -> nope initPos text0 mo
         , rej = \mo -> nope initPos text0 mo
         , err = nope
         }
      where
        nope :: pos -> Text -> Maybe String -> Res pos a
        nope pos text mes = Left (pos, text, mes)
        yes pos text a =
          case scanTok text of
            Nothing -> Right a
            Just{} -> nope pos text Nothing


    run :: forall a b. pos -> Text -> Par pos tok a -> K4 pos a b -> Res pos b
    run p t par k@K4{eps,succ,fail,rej,err} = case par of

      Position -> eps p

      Ret x -> eps x

      Fail -> fail Nothing
      Expect mes -> fail (Just mes)

      Reject mes -> rej (Just mes)

      Satisfy pred -> do
        case scanTok t of
          Nothing -> fail Nothing
          Just (c,t) ->
            case pred c of
              Just a -> succ (tickPos p c) t a
              Nothing -> fail Nothing

      NoError par -> do
        run p t par K4 { eps = eps
                       , succ = succ
                       , fail = fail
                       , rej = rej
                       , err = \_p _t -> fail
                       }

      Alt par1 par2 -> do
        run p t par1 K4{ eps = \a1 ->
                           run p t par2 K4{ eps = \_ -> undefined $ eps a1 -- left biased -- ever happen?
                                          , succ
                                          , fail = \_ -> eps a1
                                          , rej = \_ -> undefined $ eps a1
                                          , err
                                          }
                       , succ
                       , fail = \mo1 ->
                           run p t par2 K4{ eps
                                          , succ
                                          , fail = \mo2 -> fail (combineMo mo1 mo2)
                                          , rej
                                          , err
                                          }
                       , rej
                       , err
                       }
          where
            combineMo :: Maybe String -> Maybe String -> Maybe String
            combineMo = \case
              Nothing -> id
              Just mo -> \case
                Nothing -> Just mo
                Just _mo2 -> undefined $ Just mo -- left biased -- ever happen?

      Bind par f -> do
        let (pHOLD,tHOLD) = (p,t)
        run p t par K4{ eps = \a -> run p t (f a) k
                      , succ = \p t a ->
                                 run p t (f a) K4{ eps = succ p t -- consume
                                                 , succ
                                                 , fail = err p t  -- fail->error
                                                 , rej = err pHOLD tHOLD  -- fail->error
                                                 , err
                                                 }
                      , fail
                      , rej
                      , err
                      }
