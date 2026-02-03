
module JustLine(Pos,toLine,ofLine,initPos,tickPos,Mode(..),showPos) where

import Text.Printf (printf)

type Pos = Int

toLine :: Int -> Pos
toLine x = x

ofLine :: Pos -> Int
ofLine x = x

initPos :: Pos
initPos = 1

tickPos :: Pos -> Char -> Pos
tickPos line  = \case '\n' -> 1 + line; _ -> line

data Mode = Brief | Verbose

showPos :: Mode -> Pos -> String
showPos _ line = printf "[line %d]" line
