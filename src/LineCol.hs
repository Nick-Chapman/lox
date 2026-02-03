
module LineCol (Pos,toLine,ofLine,initPos,tickPos,Mode(..),showPos) where

import Text.Printf (printf)

data Pos = Pos { line :: Int, col :: Int }

toLine :: Pos -> Int
toLine Pos {line} = line

ofLine :: Int -> Pos
ofLine line = Pos { line, col = 0 }

initPos :: Pos
initPos = Pos { line = 1, col = 0 }

tickPos :: Pos -> Char -> Pos
tickPos Pos {line,col} = \case
  '\n' -> Pos { line = 1 + line, col = 0 }
  _ -> Pos { line, col = col + 1 }

data Mode = Brief | Verbose

showPos :: Mode -> Pos -> String
showPos = \case
  Brief -> \Pos{line} -> printf "[line %d]" line
  Verbose -> \Pos{line,col} -> printf "[line %d, column %d]" line col
