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
import Data.Text (pack)
import qualified Data.Time as Time
import Sensei.App
import Sensei.CLI
import qualified Sensei.Client as Client
import Sensei.IO (readConfig)
import Sensei.API(userDefinedFlows)
import Sensei.Wrapper
import System.Directory
import System.Environment
import System.IO

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
    "ep" -> do
      res <- try @Client.ClientError $ send io (Client.getUserProfileC $ realUser)
      let flows = case res of
                    Left _err -> Nothing
                    Right profile -> userDefinedFlows profile
      opts <- parseSenseiOptions flows
      ep config opts realUser st currentDir
    "sensei-exe" -> do
      -- TODO this is clunky
      configDir <- fromMaybe "." <$> lookupEnv "SENSEI_SERVER_CONFIG_DIR"
      startServer configDir
    _ -> do
      res <- tryWrapProg io realUser prog progArgs currentDir
      handleWrapperResult prog res
