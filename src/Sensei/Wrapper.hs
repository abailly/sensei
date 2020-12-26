{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.Wrapper where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time (UTCTime, diffUTCTime)
import Sensei.Client (ClientMonad, getUserProfileC, postEventC)
import Sensei.Flow
import Sensei.User
import System.Exit (ExitCode (..))

-- | A record of functions exposing IO actions needed for wrapping program executions
data WrapperIO m = WrapperIO
  { runProcess :: String -> [String] -> m ExitCode,
    getCurrentTime :: m UTCTime,
    send :: forall a. ClientMonad a -> m a,
    fileExists :: FilePath -> m Bool
  }

defaultCommands :: Map.Map String String
defaultCommands =
  Map.fromList $
    [ ("docker", "/usr/local/bin/docker"),
      ("dotnet", "/usr/local/share/dotnet/dotnet"),
      ("npm", "/usr/local/bin/npm"),
      ("az", "/usr/local/bin/az"),
      ("git", "/usr/bin/git")
    ]

-- | Possible errors when trying to run an alias
data WrapperError
  = UnMappedAlias String
  | NonExistentAlias String String
  deriving (Eq, Show)

tryWrapProg ::
  (Monad m) =>
  WrapperIO m ->
  String ->
  String ->
  [String] ->
  FilePath ->
  m (Either WrapperError ExitCode)
tryWrapProg io@WrapperIO {..} curUser prog args currentDir = do
  commands <- fromMaybe defaultCommands . userCommands <$> send (getUserProfileC $ pack curUser)
  case Map.lookup prog commands of
    Just realPath -> do
      isFile <- fileExists realPath
      if isFile
        then Right <$> wrapProg io curUser realPath args currentDir
        else pure $ Left (NonExistentAlias prog realPath)
    Nothing -> pure $ Left (UnMappedAlias prog)

wrapProg ::
  (Monad m) =>
  WrapperIO m ->
  String ->
  String ->
  [String] ->
  FilePath ->
  m ExitCode
wrapProg WrapperIO {..} curUser realProg progArgs currentDir = do
  st <- getCurrentTime
  ex <- runProcess realProg progArgs
  en <- getCurrentTime
  send (postEventC $ EventTrace $ Trace (pack curUser) en currentDir (pack realProg) (fmap pack progArgs) (toInt ex) (diffUTCTime en st))
  pure ex

toInt :: ExitCode -> Int
toInt ExitSuccess = 0
toInt (ExitFailure ex) = ex
