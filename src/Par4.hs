-- | 4-value Parser Combinators
module Par4 (Par,parse,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,skip,separated,terminated,many,some,digit,dot,noError,Pos(..),position,reject) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
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

type Res a = Either (Expect,Cursor) (a,Cursor)

data Cursor = Cursor { index :: Int }

data Expect = Expect { posOpt :: Maybe Pos, message :: String }

errorAt :: Expect -> String -> Cursor -> String
errorAt Expect {posOpt,message} chars0 x = do
  let Cursor{index=i0} = x
  let
    (pos,atMsg) =
      case posOpt of
        Nothing -> do
          let pos = mkPos chars0 i0
          let message = if i0 < length chars0 then (" at " ++ show (chars0 !! i0)) else ""
          (pos,message ++ ": ")
        Just pos -> (pos,"")
  let Pos{line,col} = pos
  let andCol = False
  let colS = if andCol then "." ++ show col else ""
  printf "[line %d%s] Error%s%s." line colS atMsg message

mkPos :: String -> Int -> Pos
mkPos chars0 i = Pos {line,col}
  where
    line :: Int = 1 + length [ () | c <- take i chars0, c == '\n' ]
    col :: Int = length (takeWhile (/= '\n') (reverse (take i chars0)))

mkCursor :: Int -> Cursor
mkCursor index = Cursor {index}

data Pos = Pos { line :: Int, col :: Int } deriving (Eq,Ord)

instance Show Pos where
  show Pos{line,col} = show line ++ "'" ++ show col

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
  , succ :: Cursor -> [Char] -> a -> Res b
  , fail :: Res b
  , err :: Expect -> Cursor -> [Char] -> Res b
  }

parse :: String -> Par a -> String -> Either String a
parse something parStart chars0  = do

  case (run (mkCursor 0) chars0 parStart kFinal) of
    Left (expect,x) -> Left (errorAt expect chars0 x)
    Right (a,x@Cursor{index=i}) -> do
      if i == length chars0 then Right a else
        Left (errorAt the_x0 chars0 x)

  where

    the_x0 :: Expect -- TODO: cleanup
    the_x0 = Expect Nothing something

    kFinal = K4 { eps = \a -> Right (a,mkCursor 0)
                , succ = \i _ a -> Right (a,i)
                , fail = Left (the_x0, mkCursor 0)
                , err = \expect i _ -> Left (expect,i)
                }

    run :: Cursor -> [Char] -> Par a -> K4 a b -> Res b
    run i chars par k@K4{eps,succ,fail,err} = case par of

      Position -> do
        let Cursor{index} = i
        let pos = mkPos chars0 index
        eps pos

      Ret a -> eps a

      Fail -> fail

      Reject pos message -> err (Expect (Just pos) message) i chars

      Satisfy pred -> do
        case chars of
          [] -> fail
          c:chars -> do
            let Cursor {index} = i
            let i' = i { index = 1 + index }
            if pred c then succ i' chars c else fail

      NoError par -> do
        run i chars par K4 { eps = eps
                           , succ = succ
                           , fail = fail
                           , err = \_expect _ _ -> fail
                           }

      Alt p1 p2 -> do
        run i chars p1 K4{ eps = \a1 ->
                             run i chars p2 K4{ eps = \_ -> eps a1 -- left biased
                                              , succ
                                              , fail = eps a1
                                              , err
                                              }
                         , succ
                         , fail = run i chars p2 k
                         , err
                         }

      Bind par f -> do
        run i chars par K4{ eps = \a -> run i chars (f a) k
                          , succ = \i chars a ->
                                     run i chars (f a) K4{ eps = \a -> succ i chars a -- consume
                                                         , succ
                                                         , fail = err the_x0 i chars -- fail->error
                                                         , err
                                                         }
                          , fail
                          , err
                          }
