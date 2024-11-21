{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception (try)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Time as Time
import Sensei.API (userDefinedFlows)
import Sensei.App (startServer)
import Sensei.CLI (ep, runOptionsParser)
import qualified Sensei.Client as Client
import Sensei.IO (readConfig)
import qualified Sensei.Server as Server
import Sensei.Wrapper (
  WrapperIO (send),
  handleWrapperResult,
  tryWrapProg,
  wrapperIO,
 )
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.IO (
  BufferMode (NoBuffering),
  hPutStrLn,
  hSetBuffering,
  stderr,
  stdout,
 )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  currentDir <- pack <$> getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- Time.getCurrentTime
  curUser <- fromMaybe "" <$> lookupEnv "USER"
  config <- readConfig

  let io = wrapperIO config
      realUser = fromMaybe (pack curUser) (Client.configUser config)

  case prog of
    "ep" -> runClient io progArgs realUser config st currentDir
    "sensei-exe" -> do
      -- TODO this is clunky
      configDir <- fromMaybe "." <$> lookupEnv "SENSEI_SERVER_CONFIG_DIR"
      opts <- Server.parseSenseiOptions
      case opts of
        Server.ServerOptions _ -> startServer configDir
        Server.ClientOptions arguments -> runClient io arguments realUser config st currentDir
    _ -> do
      res <- tryWrapProg io realUser prog progArgs currentDir
      handleWrapperResult prog res

runClient ::
  WrapperIO Client.SenseiClientConfig IO ->
  [String] ->
  Text ->
  Client.SenseiClientConfig ->
  Time.UTCTime ->
  Text ->
  IO ()
runClient io arguments realUser config st currentDir = do
  res <- try @Client.ClientError $ send io Client.getUserProfileC
  let flows = case res of
        Left _err -> Nothing
        Right profile -> userDefinedFlows profile
  case runOptionsParser flows arguments of
    Left err -> hPutStrLn stderr (unpack err) >> exitFailure
    Right opts -> ep config opts realUser st currentDir
