{-# LANGUAGE OverloadedStrings #-}
module Sensei.TestHelper where

import Control.Exception.Safe(bracket)
import Sensei.App
import System.Directory
import qualified Data.Aeson as A
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import System.Posix.Temp (mkstemp)
import Test.Hspec.Wai as W
import System.IO

withTempFile :: (String -> IO a) -> IO a
withTempFile =
  bracket mkTempFile removePathForcibly
  where
    mkTempFile = mkstemp "test-sensei" >>= \(fp, h) -> hClose h >> pure fp

mkApp :: IO Application
mkApp = withTempFile $ \file -> pure (senseiApp file)

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession () SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)

getJSON :: ByteString -> WaiSession () SResponse
getJSON path = request "GET" path [("Accept", "application/json")] mempty

bodyContains :: ByteString -> MatchBody
bodyContains fragment =
  MatchBody $
    \_ body ->
      if fragment `isInfixOf` toStrict body
        then Nothing
        else Just ("String " <> unpack (decodeUtf8 fragment) <> " not found in " <> unpack (decodeUtf8 $ toStrict body))
