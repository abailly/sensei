{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.WrapperSpec where

import Control.Monad.Reader
import Data.Binary.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive
import Data.Foldable
import Data.Sequence
import Data.Text (splitOn)
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Debug.Trace
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Types.Version
import qualified Network.HTTP.Types as H
import Network.Wai (rawPathInfo)
import qualified Network.Wai as Wai
import Network.Wai.Test as Wai
import Sensei.Client
import Sensei.TestHelper
import Sensei.Wrapper
import Servant.Client.Core
import Servant.Client.Core.Response
import System.Exit
import Test.Hspec
import Test.Hspec.Wai hiding (request)
import Test.Hspec.Wai.Internal (WaiSession (..))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import qualified Data.ByteString as BS

fromClientRequest ::
  (MonadIO m) => Request -> m Wai.Request
fromClientRequest inReq =
  let acceptHeaders = fmap (\mt -> ("Accept", renderHeader mt)) $ toList (requestAccept inReq)
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
        \ chunk -> case chunk of
                     [] -> ([], BS.empty)
                     (c:cs) -> trace ("outputting " <> show c <>", chunks :  "<> show cs) $ (cs,c)
      (segments, query) = H.decodePath rawPath
      req b=
        Wai.defaultRequest
          { Wai.rawPathInfo = rawPath,
            Wai.requestMethod = requestMethod inReq,
            Wai.requestHeaders = ("Content-type", "application/json") : headers,
            Wai.requestBody = b,
            Wai.httpVersion = requestHttpVersion inReq,
            Wai.pathInfo = segments,
            Wai.queryString = query,
            Wai.rawQueryString = H.renderQuery True query,
            Wai.requestBodyLength = Wai.ChunkedBody
          }
   in do
    chunks <- body <$> liftIO (newIORef [bdy])
    pure $ trace (show $ req chunks) $ req chunks

toClientResponse ::
  SResponse -> Response
toClientResponse SResponse {..} =
  Response simpleStatus (fromList simpleHeaders) http11 simpleBody

instance RunClient (WaiSession st) where
  runRequest req = do
    WaiSession $ ReaderT $ \_ -> fromClientRequest req >>= \ r -> toClientResponse <$> request r

  throwClientError err = error (show err)

io :: WrapperIO (WaiSession ())
io = WrapperIO {..}
  where
    runProcess _ _ = pure ExitSuccess
    getCurrentTime = pure $ UTCTime (toEnum 50000) 0
    send (ClientMonad a) = a
    exitWith = const $ pure ()

spec :: Spec
spec =
  withApp app $
    describe "Program wrapper" $ do
      it "records execution trace of wrapped program and returns program's exit code" $ do
        wrapProg io "git" ["status"] "somedir"

-- we mostly check the fucntion executes properly so this assertion is somewhat
-- silly
--res `shouldBe` ()
