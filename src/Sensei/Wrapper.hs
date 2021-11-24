{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.Wrapper where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.Time as Time
import Sensei.Client (ClientConfig, ClientMonad, getUserProfileC, postEventC)
import qualified Sensei.Client as Client
import Sensei.Event(Event(..))
import Sensei.Flow
import Sensei.User
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (Inherit),
    createProcess,
    proc,
    waitForProcess,
  )

-- | A record of functions exposing IO actions needed for wrapping program executions
data WrapperIO m = WrapperIO
  { runProcess :: String -> [String] -> m ExitCode,
    getCurrentTime :: m UTCTime,
    send :: forall a. ClientMonad a -> m a,
    fileExists :: FilePath -> m Bool
  }

wrapperIO :: ClientConfig -> WrapperIO IO
wrapperIO config =
  WrapperIO
    { runProcess =
        \realProg progArgs -> do
          (_, _, _, h) <-
            createProcess
              (proc realProg progArgs)
                { std_in = Inherit,
                  std_out = Inherit,
                  std_err = Inherit
                }
          waitForProcess h,
      getCurrentTime = Time.getCurrentTime,
      send = Client.send config,
      fileExists = doesFileExist
    }

handleWrapperResult :: String -> Either WrapperError ExitCode -> IO b
handleWrapperResult prog (Left UnMappedAlias {}) = do
  hPutStrLn
    stderr
    ( "Don't know how to handle program '" <> prog
        <> "'. You can add a symlink from '"
        <> prog
        <> "' to 'sensei-exe' and configure user profile."
    )
  exitWith (ExitFailure 1)
handleWrapperResult _ (Left (NonExistentAlias al real)) = do
  hPutStrLn stderr ("Program '" <> real <> "' pointed at by '" <> al <> "' does not exist, check user profile configuration.")
  exitWith (ExitFailure 1)
handleWrapperResult _ (Right ex) = exitWith ex

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
  Text ->
  String ->
  [String] ->
  Text ->
  m (Either WrapperError ExitCode)
tryWrapProg io@WrapperIO {..} curUser prog args currentDir = do
  commands <- fromMaybe defaultCommands . userCommands <$> send (getUserProfileC curUser)
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
  Text ->
  String ->
  [String] ->
  Text ->
  m ExitCode
wrapProg WrapperIO {..} curUser realProg progArgs currentDir = do
  st <- getCurrentTime
  ex <- runProcess realProg progArgs
  en <- getCurrentTime
  send (postEventC (UserName curUser) [EventTrace $ Trace curUser en currentDir (pack realProg) (fmap pack progArgs) (toInt ex) (diffUTCTime en st)])
  pure ex

toInt :: ExitCode -> Int
toInt ExitSuccess = 0
toInt (ExitFailure ex) = ex
