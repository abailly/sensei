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

import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Time as Time
import Sensei.App
import Sensei.CLI
import qualified Sensei.Client as Client
import Sensei.Wrapper
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (Inherit),
    createProcess,
    proc,
    waitForProcess,
  )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- Time.getCurrentTime
  curUser <- fromMaybe "" <$> lookupEnv "USER"

  case prog of
    "ep" -> do
      profile <- send (Client.getUserProfileC $ pack curUser)
      opts <- parseSenseiOptions profile
      ep dummyConfig opts (pack curUser) st (pack currentDir)
    "sensei-exe" -> startServer
    _ -> do
      res <- tryWrapProg io curUser prog progArgs currentDir
      handleWrapperResult prog res
  where
    io :: WrapperIO IO
    io = WrapperIO {..}

    dummyConfig = Client.ClientConfig "localhost" 23456 Nothing

    runProcess =
      \realProg progArgs -> do
        (_, _, _, h) <-
          createProcess
            (proc realProg progArgs)
              { std_in = Inherit,
                std_out = Inherit,
                std_err = Inherit
              }
        waitForProcess h

    getCurrentTime = Time.getCurrentTime

    send = Client.send dummyConfig

    fileExists = doesFileExist

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
