{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.ClientSpec where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Network.HTTP.Types (http11, status200)
import Sensei.API (currentVersion)
import Sensei.Client (ClientConfig (..), getVersionsC, unClient)
import Sensei.TestHelper (validAuthToken, validSerializedToken)
import Sensei.Version (Versions (..), senseiVersion)
import Servant.Client.Core (Request, Response, ResponseF (Response), requestHeaders)
import Servant.Client.Core.RunClient (RunClient (..))
import Test.Hspec

newtype TestClient a = TestClient {testClient :: Writer [Request] a}
  deriving (Functor, Applicative, Monad, MonadWriter [Request])

instance RunClient TestClient where
  runRequestAcceptStatus _st req = tell [req] >> pure response

  throwClientError err = error (show err)

response :: Response
response = Response status200 (fromList [("Content-type", "application/json")]) http11 (encode $ Versions senseiVersion senseiVersion currentVersion currentVersion)

spec :: Spec
spec = describe "ClientMonad" $ do
  it "sets Host, Origin and X-API-Version header from ClientConfig info" $ do
    let config = ClientConfig "http://1.2.3.4:1234" Nothing False
        [request] = snd $ runWriter $ testClient (runReaderT (unClient getVersionsC) config)

    toList (requestHeaders request)
      `shouldContain` [ (mk "Host", "1.2.3.4:1234"),
                        (mk "Origin", "http://1.2.3.4:1234"),
                        (mk "X-API-Version", "0.31.0")
                      ]

  it "sets Authorization header from ClientConfig info given token is set" $ do
    let config = ClientConfig "http://1.2.3.4:1234" (Just validSerializedToken) False
        [request] = snd $ runWriter $ testClient (runReaderT (unClient getVersionsC) config)

    toList (requestHeaders request) `shouldContain` [(mk "Authorization", "Bearer " <> LBS.toStrict validAuthToken)]
