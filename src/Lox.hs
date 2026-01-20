module Lox (main) where

import Ast (Stat)
import Compiler qualified (executeTopDecls)
import Data.Text qualified as Text
import Interpreter qualified (executeTopDecls)
import Parser qualified (tryParse)
import Resolver qualified (resolveTop)
import Runtime (Eff,runEffect)
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (stdout,stderr,hFlush,hPutStrLn,hSetBinaryMode)
import System.IO.Binary (readBinaryFile)

main :: IO ()
main = do
  Config{files,mode} <- parseArgs <$> getArgs
  case files of
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
              Runtime.runEffect putOut (interpret mode decls) >>= \case
                Right _globals' -> do
                  -- if/when support repl, we have the updated globals in hand
                  pure ()
                Left err -> do
                  abort 70 [err]
        where


interpret :: Mode -> [Stat] -> Eff ()
interpret mode stats =
  case mode of
    ModeTree -> do _ <- Interpreter.executeTopDecls stats; pure ()
    ModeBCI -> Compiler.executeTopDecls stats

data Mode = ModeTree | ModeBCI

data Config = Config { files :: [String], mode :: Mode }

parseArgs :: [String] -> Config
parseArgs = loop Config { files = [], mode = defaultMode }
  where
    defaultMode = ModeTree
    loop acc = \case
      [] -> acc
      "-tree":xs -> loop acc { mode = ModeTree } xs
      "-bci":xs -> loop acc { mode = ModeBCI } xs
      flag@('-':_):_ -> error ("unknown flag: " ++ flag)
      file:xs -> loop acc { files = file : files acc } xs


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
