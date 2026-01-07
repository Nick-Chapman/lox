module Lox (main) where

import Data.Text qualified as Text
import Interpreter qualified (executeTopDecls)
import Parser qualified (tryParse)
import Runtime (runEffect)
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (stdout,stderr,hFlush,hPutStrLn,hSetBinaryMode)
import System.IO.Binary (readBinaryFile)

main :: IO ()
main = do
  getArgs >>= \case
    [] -> error "no repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- Text.pack <$> readBinaryFile filename
      case (Parser.tryParse contents) of
        Left err -> abort 65 err
        Right decls -> do
          Runtime.runEffect putOut (Interpreter.executeTopDecls decls) >>= \case
            Right _globals' -> do
              -- if/when support repl, we have the updated globals in hand
              pure ()
            Left err -> do
              abort 70 err

abort :: Int -> String -> IO a
abort code mes = do
  putErr mes
  exitWith (ExitFailure code)

putErr :: String -> IO ()
putErr s = do
  hSetBinaryMode stderr True
  hPutStrLn stderr s
  hFlush stderr

putOut :: String -> IO ()
putOut s = do
  hSetBinaryMode stdout True
  hPutStrLn stdout s
  hFlush stdout
