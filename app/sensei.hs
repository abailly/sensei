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

import Data.Text (pack)
import qualified Data.Time as Time
import Sensei.App
import Sensei.CLI
import qualified Sensei.Client as Client
import Sensei.Wrapper
import System.Directory
import System.Environment
import qualified System.Exit as Exit
import System.IO
import System.Posix.User
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (Inherit),
    createProcess,
    proc,
    waitForProcess,
  )

io :: WrapperIO IO
io =
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
      send = Client.send,
      fileExists = doesFileExist
    }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- Time.getCurrentTime
  curUser <- getLoginName

  case prog of
    "ep" -> do
      profile <- Client.send (Client.getUserProfileC $ pack curUser)
      opts <- parseSenseiOptions profile
      ep opts (pack curUser) st (pack currentDir)
    "sensei-exe" -> startServer
    _ -> do
      res <- tryWrapProg io curUser prog progArgs currentDir
      case res of
        Left UnMappedAlias{} -> do
          hPutStrLn
            stderr
            ( "Don't know how to handle program '" <> prog
                <> "'. You can add a symlink from '"
                <> prog
                <> "' to 'sensei-exe' and configure user profile."
            )
          Exit.exitWith (Exit.ExitFailure 1)
        Left (NonExistentAlias al real) -> do
          hPutStrLn stderr ("Program '" <> real <> "' pointed at by '" <> al <> "' does not exist, check user profile configuration.")
          Exit.exitWith (Exit.ExitFailure 1)
        Right ex -> Exit.exitWith ex
