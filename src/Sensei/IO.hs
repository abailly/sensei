module Sensei.IO where

import System.Directory

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
