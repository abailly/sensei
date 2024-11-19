{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn #-}

module Sensei.ClientSpec where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Foldable (toList)
import Data.Sequence (fromList)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (http11, status200)
import Sensei.Client (SenseiClientConfig (..), defaultConfig, getVersionsC, unClient)
import Sensei.Server (AuthenticationToken (..), SerializedToken (..))
import Sensei.TestHelper (authTokenFor, sampleKey)
import Sensei.Version (Version (..), Versions (..), currentVersion, senseiVersion, senseiVersionLBS, showVersion)
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
  let aConfig = SenseiClientConfig "http://1.2.3.4:1234" Nothing False Nothing Nothing
  it "sets Host, Origin and X-API-Version header from ClientConfig info" $ do
    let [request] = snd $ runWriter $ testClient (runReaderT (unClient getVersionsC) aConfig)

    toList (requestHeaders request)
      `shouldContain` [ (mk "Host", "1.2.3.4:1234")
                      , (mk "Origin", "http://1.2.3.4:1234")
                      , (mk "X-API-Version", senseiVersionLBS)
                      ]

  it "sets Authorization header from ClientConfig info given token is set" $ do
    token <- authTokenFor (AuthToken "" 1) sampleKey
    let configWithAuth = aConfig{authToken = Just . SerializedToken . LBS.toStrict $ token}
        [request] = snd $ runWriter $ testClient (runReaderT (unClient getVersionsC) configWithAuth)

    toList (requestHeaders request) `shouldContain` [(mk "Authorization", "Bearer " <> LBS.toStrict token)]

  it "sets X-API-Version header from ClientConfig info given it is set" $ do
    let someVersion = Version [3, 4] ["foo"]
        configWithAuth = aConfig{apiVersion = Just someVersion}
        [request] = snd $ runWriter $ testClient (runReaderT (unClient getVersionsC) configWithAuth)

    toList (requestHeaders request) `shouldContain` [(mk "X-API-Version", encodeUtf8 $ Text.pack $ showVersion someVersion)]

  describe "Client Config" $ do
    -- it "can serialise/deserialise to/from JSON" $
    --   lawsCheck (jsonLaws (Proxy @ClientConfig))

    it "can deserialise version-less JSON" $ do
      serializedToken <- SerializedToken . LBS.toStrict <$> authTokenFor (AuthToken "" 1) sampleKey
      let jsonProfile =
            "{\"serverUri\":\"http://localhost:23456\",\
            \ \"startServerLocally\":true, \
            \ \"authToken\":"
              <> encode serializedToken
              <> "}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultConfig{authToken = Just serializedToken}

    it "can deserialise JSON with userName but without authToken" $ do
      let jsonProfile =
            "{\"serverUri\":\"http://localhost:23456\",\
            \ \"startServerLocally\":true, \
            \ \"configUser\":\"foo\"}"
      eitherDecode jsonProfile
        `shouldBe` Right defaultConfig{configUser = Just "foo"}
