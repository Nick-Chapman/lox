module Lox (main) where

import Data.Text qualified as Text
import Interpreter qualified (executeTopDecls)
import Compiler qualified (executeTopDecls)
import Parser qualified (tryParse)
import Resolver qualified (resolveTop)
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
      case Parser.tryParse contents of
        Left err -> abort 65 [err]
        Right decls -> do
          case Resolver.resolveTop decls of
            errs@(_:_)-> abort 65 errs
            [] ->
              Runtime.runEffect putOut (_interpretBC decls) >>= \case
                Right _globals' -> do
                  -- if/when support repl, we have the updated globals in hand
                  pure ()
                Left err -> do
                  abort 70 [err]
        where
          _interpret = Interpreter.executeTopDecls
          _interpretBC = Compiler.executeTopDecls

abort :: Int -> [String] -> IO a
abort code errs = do
  mapM_ putErr errs
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
