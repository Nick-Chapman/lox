module Runtime (runtimeError,abort) where

import Par4 (Pos(..))
import System.Exit(ExitCode(..),exitWith)
import System.IO (stderr,hFlush,hPutStrLn)
import Text.Printf (printf)

runtimeError :: Pos -> String -> IO a
runtimeError Pos{line} mes = abort 70 (printf "%s\n[line %d]" mes line)

abort :: Int -> String -> IO a
abort code mes = do
  putErr mes
  exitWith (ExitFailure code)

putErr :: String -> IO ()
putErr s = do
  hPutStrLn stderr s
  hFlush stderr
  pure ()
