module Sensei.Wrapper where

import Data.Time ( diffUTCTime, getCurrentTime, UTCTime )
import Data.Text(pack)
import Sensei.Client ( traceC, send )
import System.Exit
import System.Process
    ( createProcess,
      proc,
      waitForProcess,
      CreateProcess(std_in, std_out, std_err),
      StdStream(Inherit) )
import Sensei.Flow

wrapProg :: String -> [String] -> UTCTime -> FilePath -> IO ()
wrapProg realProg progArgs st currentDir = do
  (_, _, _, procHandle) <-
    createProcess
      (proc realProg progArgs)
        { std_in = Inherit,
          std_out = Inherit,
          std_err = Inherit
        }

  ex <- waitForProcess procHandle
  en <- getCurrentTime
  send (traceC $ Trace en currentDir (pack realProg) (fmap pack progArgs) (toInt ex) (diffUTCTime en st) currentVersion)
  exitWith ex

toInt :: ExitCode -> Int
toInt ExitSuccess = 0
toInt (ExitFailure ex) = ex
