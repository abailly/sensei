module Sensei.Wrapper where

import Data.Time ( diffUTCTime, getCurrentTime, UTCTime )
import Sensei.API ( Trace(Trace) )
import Data.Text(pack)
import Sensei.Client ( traceC, send )
import System.Exit ( exitWith )
import System.Process
    ( createProcess,
      proc,
      waitForProcess,
      CreateProcess(std_in, std_out, std_err),
      StdStream(Inherit) )

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
  send (traceC $ Trace en currentDir (pack realProg) (fmap pack progArgs) ex (diffUTCTime en st))
  exitWith ex
