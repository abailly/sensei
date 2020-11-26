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
import qualified Data.Time as Time
import Sensei.App
import Sensei.CLI
import qualified Sensei.Client as Client
import Sensei.Wrapper
import System.Directory
import System.Environment
import qualified System.Exit as Exit
import System.FilePath
import System.IO
import System.Posix.User
import System.Process
    (waitForProcess,  proc,
      createProcess,
      CreateProcess(std_in, std_out, std_err),
      StdStream(Inherit) )

io :: WrapperIO IO
io =
  WrapperIO { runProcess =
                \ realProg progArgs -> do
                  (_,_,_,h) <- createProcess
                    (proc realProg progArgs)
                    { std_in = Inherit,
                      std_out = Inherit,
                      std_err = Inherit
                    }
                  waitForProcess h ,
              getCurrentTime = Time.getCurrentTime,
              send = Client.send,
              exitWith = Exit.exitWith
            }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- Time.getCurrentTime
  curUser <- getLoginName

  case prog of
    "git" -> wrapProg io "/usr/bin/git" progArgs currentDir
    "stak" -> wrapProg io (homeDir </> ".local/bin/stack") progArgs currentDir
    "docker" -> wrapProg io "/usr/local/bin/docker" progArgs currentDir
    "dotnet" -> wrapProg io  "/usr/local/share/dotnet/dotnet" progArgs currentDir
    "npm" -> wrapProg io "/usr/local/bin/npm" progArgs currentDir
    "az" -> wrapProg io "/usr/local/bin/az" progArgs currentDir
    "ep" -> do
      profile <- Client.send (Client.getUserProfileC $ pack curUser)
      opts <- parseSenseiOptions profile
      flowAction opts (pack curUser) st (pack currentDir)
    "sensei-exe" -> startServer
    _ -> hPutStrLn stderr ("Don't know how to handle program " <> prog) >> exitWith io (Exit.ExitFailure 1)
