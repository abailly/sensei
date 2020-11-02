{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sensei.TestHelper where

import Control.Exception.Safe(bracket, catch, finally, throwIO)
import Sensei.App
import Control.Concurrent(threadDelay)
import System.Directory
import qualified Data.Aeson as A
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Text(unpack)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Test.Hspec
import System.Posix.Temp (mkstemp)
import Test.Hspec.Wai as W
import System.IO

withApp :: SpecWith ((), Application) -> Spec
withApp = around mkApp

mkApp :: ActionWith ((), Application) -> IO ()
mkApp act = do
  file <- mkTempFile
  let app = senseiApp file
  act ((), app)
    `finally` removePathForcibly file
  where
    mkTempFile = mkstemp "test-sensei" >>= \(fp, h) -> hClose h >> pure fp

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
