module Lox (main) where

import Interpreter qualified (execute)
import Parser qualified (parser)
import System.Environment (getArgs)
import System.IO.Binary (readBinaryFile)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> error "repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- readBinaryFile filename
      prog <- Parser.parser contents
      Interpreter.execute prog
