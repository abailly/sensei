module Sensei.IO where

import System.Directory
import Control.Monad(when)

getConfigDirectory :: IO FilePath
getConfigDirectory = do
  home <- getXdgDirectory XdgConfig "sensei"
  exists <- doesDirectoryExist home
  when (not exists) $ createDirectory home
  pure home

getDataDirectory :: IO FilePath
getDataDirectory = do
  home <- getXdgDirectory XdgData "sensei"
  exists <- doesDirectoryExist home
  when (not exists) $ createDirectory home
  pure home
