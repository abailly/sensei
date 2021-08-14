{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception(try)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack, Text)
import qualified Data.Time as Time
import Sensei.App
import Sensei.CLI
import qualified Sensei.Client as Client
import qualified Sensei.Server as Server
import Sensei.IO (readConfig)
import Sensei.API(userDefinedFlows)
import Sensei.Wrapper
import System.Directory
import System.Environment
import System.IO
import System.Exit(exitFailure)

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
        Server.ClientOptions args -> runClient io args realUser config st currentDir 
    _ -> do
      res <- tryWrapProg io realUser prog progArgs currentDir
      handleWrapperResult prog res

runClient :: WrapperIO IO
  -> [String]
                   -> Text
                   -> Client.ClientConfig
                   -> Time.UTCTime
                   -> Text
                   -> IO ()
runClient io args realUser config st currentDir = do
      res <- try @Client.ClientError $ send io (Client.getUserProfileC $ realUser)
      let flows = case res of
                    Left _err -> Nothing
                    Right profile -> userDefinedFlows profile
      case runOptionsParser flows args of
        Left err -> hPutStrLn stderr (unpack err) >> exitFailure
        Right opts -> ep config opts realUser st currentDir
