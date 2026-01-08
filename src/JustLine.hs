
module JustLine(Pos,initPos,tickPos,showPos) where

import Text.Printf (printf)

type Pos = Int

initPos :: Pos
initPos = 1

tickPos :: Pos -> Char -> Pos
tickPos line  = \case '\n' -> 1 + line; _ -> line

showPos :: Pos -> String
showPos line = printf "[line %d]" line
