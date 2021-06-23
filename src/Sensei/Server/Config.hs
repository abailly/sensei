{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | Configuration of sensei server
module Sensei.Server.Config where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import GHC.Generics
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data Env = Dev | Prod
  deriving (Eq, Show)

readEnv ::
  String -> Maybe Env
readEnv s =
  case fmap toLower s of
    "dev" -> pure Dev
    "development" -> pure Dev
    "prod" -> pure Prod
    "production" -> pure Prod
    _ -> Nothing

readPort :: Maybe String -> Int
readPort Nothing = 23456
readPort (Just portString) =
  case reads portString of
    (p, []) : _ -> p
    _ -> error ("invalid environment variable SENSEI_SERVER_PORT " <> portString)

data ServerConfig = ServerConfig {serverPort :: Int, serverHost :: String}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

defaultConfig :: ServerConfig
defaultConfig = ServerConfig {serverPort = 23456, serverHost = "127.0.0.1"}

readServerConfig :: FilePath -> IO (Maybe ServerConfig)
readServerConfig configDir = do
  let serverConfig = configDir </> "server.json"
  configExists <- doesFileExist serverConfig
  if configExists
    then
      LBS.readFile serverConfig
        >>= ( \case
                Left _err -> pure Nothing
                Right conf -> pure (Just conf)
            )
          . eitherDecode
    else pure Nothing
