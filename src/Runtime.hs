module Runtime (runtimeError,abort,putOut) where

import Par4 (Pos(..))
import System.Exit(ExitCode(..),exitWith)
import System.IO (stdout,stderr,hFlush,hPutStrLn,hSetBinaryMode)
import Text.Printf (printf)

runtimeError :: Pos -> String -> IO a
runtimeError Pos{line} mes = abort 70 (printf "%s\n[line %d] in script" mes line)

abort :: Int -> String -> IO a
abort code mes = do
  putErr mes
  exitWith (ExitFailure code)

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr
  pure ()

putOut :: String -> IO ()
putOut s = do
  hSetBinaryMode stdout True
  hPutStrLn stdout s
  hFlush stdout
  pure ()
