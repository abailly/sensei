{-# LANGUAGE RankNTypes #-}

module Sensei.IO where

import Data.Time (UTCTime)
import Sensei.Client.Monad (ClientMonad)
import System.Directory
import System.Exit (ExitCode (..))

-- | A record of functions exposing IO actions needed for wrapping program executions
data WrapperIO m = WrapperIO
  { runProcess :: String -> [String] -> m ExitCode,
    getCurrentTime :: m UTCTime,
    send :: forall a. ClientMonad a -> m a,
    fileExists :: FilePath -> m Bool
  }

getConfigDirectory :: IO FilePath
getConfigDirectory = do
  home <- getXdgDirectory XdgConfig "sensei"
  createDirectoryIfMissing True home
  pure home

getDataDirectory :: IO FilePath
getDataDirectory = do
  home <- getXdgDirectory XdgData "sensei"
  createDirectoryIfMissing True home
  pure home
