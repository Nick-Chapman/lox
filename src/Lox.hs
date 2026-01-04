module Lox (main) where

import Interpreter qualified (execute)
import Parser qualified (parser)
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> error "repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- readFile filename
      prog <- Parser.parser contents
      Interpreter.execute prog
