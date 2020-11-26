{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Sensei.Wrapper where

import Data.Time ( diffUTCTime, UTCTime )
import Data.Text(pack)
import Sensei.Client ( traceC, ClientMonad )
import System.Exit(ExitCode(..))
import Sensei.Flow

-- | A record of functions exposing IO actions needed for wrapping program executions
data WrapperIO m =
  WrapperIO { runProcess :: String -> [String] -> m ExitCode,
              getCurrentTime :: m UTCTime,
              send :: forall a . ClientMonad a -> m a,
              exitWith :: ExitCode -> m ()
            }

wrapProg ::
  (Monad m) =>
  WrapperIO m -> String -> [String] -> FilePath -> m ()
wrapProg WrapperIO{..} realProg progArgs currentDir = do
  st <- getCurrentTime
  ex <- runProcess realProg progArgs
  en <- getCurrentTime
  send (traceC $ Trace en currentDir (pack realProg) (fmap pack progArgs) (toInt ex) (diffUTCTime en st) currentVersion)
  exitWith ex

toInt :: ExitCode -> Int
toInt ExitSuccess = 0
toInt (ExitFailure ex) = ex
