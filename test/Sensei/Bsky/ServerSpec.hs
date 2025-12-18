{-# LANGUAGE OverloadedStrings #-}

module Sensei.Bsky.ServerSpec where

import Control.Monad.Reader (local)
import Crypto.JOSE.JWK (fromOctets)
import Data.Aeson (ToJSON, decode, encode)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (status200)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleStatus)
import Sensei.Bsky
import Sensei.Generators()
import Sensei.Bsky.Server
import Sensei.Server (SerializedToken, unToken)
import Servant.Auth.Server (defaultJWTSettings, generateSecret)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal (WaiSession (..))
import Test.QuickCheck

-- | Test credentials with a random DID
data TestCredentials = TestCredentials
  { tcIdentifier :: Text,
    tcPassword :: Text,
    tcDid :: DID
  }
  deriving (Eq, Show)

instance Arbitrary TestCredentials where
  arbitrary =
    TestCredentials
      <$> identifier
      <*> password
      <*> arbitrary
    where
      identifier = Text.pack <$> listOf1 (elements ['a' .. 'z'])
      password = Text.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9']))

-- Helper to create test app
makeTestApp :: IO Application
makeTestApp = do
  let identifier = "testuser"
      password = "testpass"
      didText = "did:plc:test123456789012345"
      audience = "did:web:bsky.social"
      validCredentials = Map.fromList [(identifier, password)]
      userDIDs = Map.fromList [(identifier, didText)]

  secret <- generateSecret
  let jwtSettings = defaultJWTSettings (fromOctets secret)

  state <- newBskyServerState validCredentials userDIDs audience jwtSettings
  pure $ bskyTestAppWithState state

withTestApp :: SpecWith (Maybe a, Application) -> Spec
withTestApp =
  around
    ( \action -> do
        app <- makeTestApp
        action (Nothing, app)
    )

spec :: Spec
spec = withTestApp $ describe "Bsky Server" $ do
  describe "Authentication flow" $ it "issues valid JWT on successful login with proper structure" $ do
    -- Login request
    let login = BskyLogin "testuser" "testpass"

    response <- postJSON "/xrpc/com.atproto.server.createSession" login

    -- Verify we get a successful response
    liftIO $ simpleStatus response `shouldBe` status200

    -- Parse response
    let Just session = decode @BskySession (simpleBody response)

    -- Decode and verify JWT structure
    let decoded = decodeAuthToken (accessJwt session)
    liftIO $ case decoded of
      Left err -> expectationFailure $ "Failed to decode token: " <> err
      Right auth@BskyAuth {exp = expTime} -> do
        -- Verify token claims
        -- FIXME: not particularly interesting as those are hardcoded in test app
        sub auth `shouldBe` "did:plc:test123456789012345"
        aud auth `shouldBe` "did:web:bsky.social"
        scope auth `shouldBe` "com.atproto.access"
        -- Verify iat and exp are reasonable
        iat auth `shouldSatisfy` (> 0)
        expTime `shouldSatisfy` (> iat auth)

  it "returns 401 given user is unauthenticated" $ do
    getJSON "/xrpc/com.atproto.repo.listRecords?repo=testuser&collection=app.bsky.feed.post"
      `shouldRespondWith` 401

  it "returns empty list of records for authenticated user" $ do
    Test.Hspec.Wai.pendingWith "Authentication does not work - need more investigation"
    -- First login to get a session
    let login = BskyLogin "testuser" "testpass"

    loginResponse <- postJSON "/xrpc/com.atproto.server.createSession" login
    let Just session = decode @BskySession (simpleBody loginResponse)

    liftIO $ putStrLn $ "Obtained session: " ++ show session

    -- Now call listRecords
    listResponse <-
      asUser (accessJwt session) $
        getJSON "/xrpc/com.atproto.repo.listRecords?repo=testuser&collection=app.bsky.feed.post"

    -- Verify we get a 200 response
    liftIO $ simpleStatus listResponse `shouldBe` status200

    -- Parse response
    let Just listResult = decode @(ListRecordsResponse BskyPost) (simpleBody listResponse)
    liftIO $ do
      cursor listResult `shouldBe` Nothing
      records listResult `shouldBe` []

postJSON :: (ToJSON a) => ByteString -> a -> WaiSession (Maybe SerializedToken) SResponse
postJSON path payload =
  defaultHeaders >>= \h -> request "POST" path h (encode payload)

getJSON :: ByteString -> WaiSession (Maybe SerializedToken) SResponse
getJSON path =
  defaultHeaders >>= \h -> request "GET" path h ""

asUser :: SerializedToken -> WaiSession (Maybe SerializedToken) a -> WaiSession (Maybe SerializedToken) a
asUser token (WaiSession s) = WaiSession $ local (const $ Just token) s

defaultHeaders :: WaiSession (Maybe SerializedToken) [HTTP.Header]
defaultHeaders = do
  token <- getState
  pure $
    [ ("Accept", "application/json"),
      ("Content-Type", "application/json")
    ]
      <> maybe [] (\u -> [("Authorization", "Bearer " <> unToken u)]) token
