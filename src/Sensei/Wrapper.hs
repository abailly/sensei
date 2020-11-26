{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Sensei.Wrapper where

import Data.Time ( diffUTCTime, UTCTime )
import Data.Text(pack)
import Sensei.Client (getUserProfileC,  traceC, ClientMonad )
import System.Exit(ExitCode(..))
import Sensei.Flow
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Sensei.User

-- | A record of functions exposing IO actions needed for wrapping program executions
data WrapperIO m =
  WrapperIO { runProcess :: String -> [String] -> m ExitCode,
              getCurrentTime :: m UTCTime,
              send :: forall a . ClientMonad a -> m a
            }

defaultCommands :: Map.Map String String
defaultCommands =
  Map.fromList $ [ ("docker", "/usr/local/bin/docker"),
                   ("dotnet", "/usr/local/share/dotnet/dotnet"),
                   ("npm", "/usr/local/bin/npm"),
                   ("az", "/usr/local/bin/az"),
                   ("git", "/usr/bin/git")
                 ]

tryWrapProg ::
  (Monad m) =>
  WrapperIO m -> String -> String -> [String] -> FilePath -> m (Either String ExitCode)
tryWrapProg io@WrapperIO{..} curUser prog args currentDir = do
  commands <- fromMaybe defaultCommands . userCommands <$> send (getUserProfileC $ pack curUser)
  case Map.lookup prog commands of
    Just realPath -> Right <$> wrapProg io realPath args currentDir
    Nothing -> pure $ Left ("Don't know how to handle program '" <> prog <> "'")

wrapProg ::
  (Monad m) =>
  WrapperIO m -> String -> [String] -> FilePath -> m ExitCode
wrapProg WrapperIO{..} realProg progArgs currentDir = do
  st <- getCurrentTime
  ex <- runProcess realProg progArgs
  en <- getCurrentTime
  send (traceC $ Trace en currentDir (pack realProg) (fmap pack progArgs) (toInt ex) (diffUTCTime en st) currentVersion)
  pure ex

toInt :: ExitCode -> Int
toInt ExitSuccess = 0
toInt (ExitFailure ex) = ex
