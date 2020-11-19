{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sensei.TestHelper where

import Control.Exception.Safe(finally)
import Sensei.App ( senseiApp )
import System.Directory ( removePathForcibly )
import qualified Data.Aeson as A
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text(unpack)
import Network.Wai.Test (SResponse)
import Test.Hspec ( around, ActionWith, Spec, SpecWith )
import System.Posix.Temp (mkstemp)
import Sensei.Version
import Servant
import Test.Hspec.Wai as W ( request, WaiSession, MatchBody(..) )
import Data.Functor(void)
import System.IO ( hClose )
import Control.Concurrent.MVar
import System.Directory

withApp :: SpecWith ((), Application) -> Spec
withApp = around mkApp

mkApp :: ActionWith ((), Application) -> IO ()
mkApp act = do
  file <- mkTempFile
  config <- mkTempDir
  signal <- newEmptyMVar
  let app = senseiApp signal file config
  act ((), app)
    `finally` removePathForcibly file >> removePathForcibly config
  where
    mkTempFile = mkstemp "test-sensei" >>= \(fp, h) -> hClose h >> pure fp
    mkTempDir = mkstemp "config-sensei" >>= \(fp, h) -> hClose h >> removePathForcibly fp >> createDirectory fp >> pure fp

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
