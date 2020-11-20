{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text(pack)
import Data.Time
import Sensei.App
import Sensei.CLI
import Sensei.Client
import Sensei.Wrapper
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.User

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- getCurrentTime
  curUser <- getLoginName

  case prog of
    "git" -> wrapProg "/usr/bin/git" progArgs st currentDir
    "stak" -> wrapProg (homeDir </> ".local/bin/stack") progArgs st currentDir
    "docker" -> wrapProg "/usr/local/bin/docker" progArgs st currentDir
    "dotnet" -> wrapProg "/usr/local/share/dotnet/dotnet" progArgs st currentDir
    "npm" -> wrapProg "/usr/local/bin/npm" progArgs st currentDir
    "az" -> wrapProg "/usr/local/bin/az" progArgs st currentDir
    "ep" -> do
      profile <- send (getUserProfileC $ pack curUser)
      opts <- parseSenseiOptions profile
      flowAction opts (pack curUser) st (pack currentDir)
    "sensei-exe" -> startServer
    _ -> hPutStrLn stderr ("Don't know how to handle program " <> prog) >> exitWith (ExitFailure 1)
