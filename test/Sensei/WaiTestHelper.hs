{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | `RunClient` instance suitable for use with WAI hspec wrapper
--  Provides
module Sensei.WaiTestHelper where

import Control.Monad.Reader
import Data.Binary.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.IORef
import Data.Sequence
import Network.HTTP.Media.RenderHeader
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Version
import qualified Network.Wai as Wai
import Network.Wai.Test as Wai
import Sensei.TestHelper (validAuthToken)
import Servant.Client.Core
import Test.Hspec
import Test.Hspec.Wai hiding (request)
import Test.Hspec.Wai.Internal (WaiSession (..))

fromClientRequest ::
  (MonadIO m) => Request -> m Wai.Request
fromClientRequest inReq =
  let acceptHeaders = (\mt -> ("Accept", renderHeader mt)) <$> toList (requestAccept inReq)
      headers = toList (requestHeaders inReq) <> acceptHeaders
      rawPath = LBS.toStrict (toLazyByteString $ requestPath inReq)
      bdy = case requestBody inReq of
        Nothing -> ""
        Just (b, _) ->
          case b of
            RequestBodyLBS lbs -> LBS.toStrict lbs
            RequestBodyBS bs -> bs
            _ -> error "don't know how to handle body source"
      body chunks =
        atomicModifyIORef chunks $
          \case
            [] -> ([], BS.empty)
            (c : cs) -> (cs, c)
      (segments, query) = H.decodePath rawPath
      req b =
        Wai.defaultRequest
          { Wai.rawPathInfo = rawPath,
            Wai.requestMethod = requestMethod inReq,
            Wai.requestHeaders =
              ("Content-type", "application/json") :
              ("Authorization", LBS.toStrict $ "Bearer " <> validAuthToken) :
              headers,
            Wai.requestBody = b,
            Wai.httpVersion = requestHttpVersion inReq,
            Wai.pathInfo = segments,
            Wai.queryString = query,
            Wai.rawQueryString = H.renderQuery True query,
            Wai.requestBodyLength = Wai.ChunkedBody
          }
   in do
        chunks <- body <$> liftIO (newIORef [bdy])
        pure $ req chunks

toClientResponse ::
  HasCallStack =>
  SResponse -> Response
toClientResponse SResponse {..} =
  Response simpleStatus (fromList simpleHeaders) http11 simpleBody

instance RunClient (WaiSession st) where
  runRequestAcceptStatus _ req = do
    WaiSession $ ReaderT $ \_ -> fromClientRequest req >>= \r -> toClientResponse <$> request r

  throwClientError err = error (show err)

isExpectedToBe ::
  (Eq a, Show a, HasCallStack) => a -> a -> WaiSession st ()
isExpectedToBe actual expected =
  if actual /= expected
    then liftIO $ expectationFailure $ show actual <> " is not " <> show expected
    else pure ()
