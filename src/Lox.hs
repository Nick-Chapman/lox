module Lox (main) where

import Ast (Stat)
import Code qualified (export)
import Compiler qualified (compile)
import Data.Text qualified as Text
import GHC.IO.Encoding (setLocaleEncoding,char8)
import Interpreter qualified (executeTopDecls)
import Parser qualified (tryParse)
import Resolver qualified (resolveTop)
import Runtime (Eff(Error))
import Runtime qualified (runEffect)
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (stdout,stderr,hFlush,hPutStrLn)
import System.IO.Binary (writeBinaryFile)
import VM (runCode)

main :: IO ()
main = do
  setLocaleEncoding char8
  Config{files,mode} <- parseArgs <$> getArgs
  case files of
    [] -> error "no repl"
    _:_:_ -> error "too any args"
    [filename] -> do
      contents <- Text.pack <$> readFile filename
      case Parser.tryParse contents of
        Left err -> abort 65 [err]
        Right decls -> do
          case Resolver.resolveTop decls of
            errs@(_:_)-> abort 65 errs
            [] ->
              runMode mode decls
        where

runMode :: Mode -> [Stat] -> IO ()
runMode mode decls =
  case mode of
    ModeTree -> do
      runEff (Interpreter.executeTopDecls decls)

    ModeBCI -> do
      case Compiler.compile decls of
        Left (pos,mes) ->
          -- i.e. for undefined variable errors, which are caught at compile time
          runEff (Runtime.Error pos mes)

        Right code -> do
          --print code -- debug
          runEff (runCode code)

    ModeExport path -> do
      case Compiler.compile decls of
        Left (pos,mes) ->
          runEff (Runtime.Error pos mes)
        Right code -> do
          --print code -- debug
          --putStr (Code.export code) -- debug
          writeBinaryFile path (Code.export code)


runEff :: Eff () -> IO ()
runEff eff = do
  Runtime.runEffect putOut eff >>= \case
    Right () -> pure ()
    Left err -> abort 70 [err]

data Mode = ModeTree | ModeBCI | ModeExport FilePath

data Config = Config { files :: [String], mode :: Mode }

parseArgs :: [String] -> Config
parseArgs = loop Config { files = [], mode = defaultMode }
  where
    defaultMode = ModeBCI
    loop acc = \case
      [] -> acc
      "-tree":xs -> loop acc { mode = ModeTree } xs
      "-bci":xs -> loop acc { mode = ModeBCI } xs
      "-export":x:xs -> loop acc { mode = ModeExport x } xs
      flag@('-':_):_ -> error ("unknown flag: " ++ flag)
      file:xs -> loop acc { files = file : files acc } xs

abort :: Int -> [String] -> IO a
abort code errs = do
  mapM_ putErr errs
  exitWith (ExitFailure code)

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr

putOut :: String -> IO ()
putOut s = do
  hPutStrLn stdout s
  hFlush stdout
