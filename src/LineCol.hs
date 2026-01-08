
module LineCol(Pos,initPos,tickPos,showPos) where

import Text.Printf (printf)

data Pos = Pos { line :: Int, col :: Int }

initPos :: Pos
initPos = Pos { line = 1, col = 0 }

tickPos :: Pos -> Char -> Pos
tickPos Pos {line,col} = \case
  '\n' -> Pos { line = 1 + line, col = 0 }
  _ -> Pos { line, col = col + 1 }

showPos :: Pos -> String
showPos Pos{line,col} = printf "[line %d, column %d]" line col
