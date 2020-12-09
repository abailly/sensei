{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.TestHelper where

import Control.Concurrent.MVar
import Control.Exception.Safe (bracket, finally)
import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Lazy (toStrict)
import Data.Functor (void)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Test (SResponse)
import Sensei.App (senseiApp)
import Sensei.Server.Config
import Sensei.Version
import Servant
import System.Directory
import System.FilePath ((<.>))
import System.IO (hClose)
import System.Posix.Temp (mkstemp)
import Test.Hspec (ActionWith, Spec, SpecWith, around)
import Test.Hspec.Wai as W (MatchBody (..), WaiSession, request)

data AppBuilder = AppBuilder {withStorage :: Bool, withEnv :: Env}

app :: AppBuilder
app = AppBuilder True Dev

withoutStorage :: AppBuilder -> AppBuilder
withoutStorage builder = builder {withStorage = False}

withApp :: AppBuilder -> SpecWith ((), Application) -> Spec
withApp builder = around (buildApp builder)

withTempFile :: (FilePath -> IO a) -> IO a
withTempFile =
  bracket mkTempFile (\fp -> removePathForcibly fp >> removePathForcibly (fp <.> "old"))

buildApp :: AppBuilder -> ActionWith ((), Application) -> IO ()
buildApp AppBuilder {..} act = do
  file <- mkTempFile
  unless withStorage $ removePathForcibly file
  config <- mkTempDir
  signal <- newEmptyMVar
  application <- senseiApp Nothing signal file config
  act ((), application)
    `finally` removePathForcibly file >> removePathForcibly config
  where
    mkTempDir = mkstemp "config-sensei" >>= \(fp, h) -> hClose h >> removePathForcibly fp >> createDirectory fp >> pure fp

mkTempFile :: IO FilePath
mkTempFile = mkstemp "test-sensei" >>= \(fp, h) -> hClose h >> pure fp

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession () SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json"), ("X-API-Version", toHeader senseiVersion)] (A.encode payload)

putJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession () SResponse
putJSON path payload = request "PUT" path [("Content-type", "application/json"), ("X-API-Version", toHeader senseiVersion)] (A.encode payload)

postJSON_ :: (A.ToJSON a) => ByteString -> a -> WaiSession () ()
postJSON_ path payload = void $ postJSON path payload

putJSON_ :: (A.ToJSON a) => ByteString -> a -> WaiSession () ()
putJSON_ path payload = void $ putJSON path payload

getJSON :: ByteString -> WaiSession () SResponse
getJSON path = request "GET" path [("Accept", "application/json"), ("X-API-Version", toHeader senseiVersion)] mempty

bodyContains :: ByteString -> MatchBody
bodyContains fragment =
  MatchBody $
    \_ body ->
      if fragment `isInfixOf` toStrict body
        then Nothing
        else Just ("String " <> unpack (decodeUtf8 fragment) <> " not found in " <> unpack (decodeUtf8 $ toStrict body))
