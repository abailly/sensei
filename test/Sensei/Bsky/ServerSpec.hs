{-# LANGUAGE OverloadedStrings #-}

module Sensei.Bsky.ServerSpec where

import Crypto.JOSE.JWK (fromOctets)
import Data.Aeson (decode)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import Network.Wai.Test (simpleBody, simpleStatus)
import Sensei.Bsky
import Sensei.Bsky.Server
import Sensei.TestHelper (postJSON)
import Servant.Auth.Server (defaultJWTSettings, generateSecret)
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck

-- | Generate a random DID identifier (did:plc:<base32>)
newtype DID = DID Text
  deriving (Eq, Show)

instance Arbitrary DID where
  arbitrary = do
    -- Generate 24 random base32 characters (matching the format)
    chars <- vectorOf 24 (elements "abcdefghijklmnopqrstuvwxyz234567")
    pure $ DID $ "did:plc:" <> Text.pack chars

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

  it "returns empty list of records for authenticated user" $ do
    -- First login to get a session
    let login = BskyLogin "testuser" "testpass"

    loginResponse <- postJSON "/xrpc/com.atproto.server.createSession" login
    let Just _session = decode @BskySession (simpleBody loginResponse)

    -- Now call listRecords
    listResponse <-
      get "/xrpc/com.atproto.repo.listRecords?repo=testuser&collection=app.bsky.feed.post"

    -- Verify we get a 200 response
    liftIO $ simpleStatus listResponse `shouldBe` status200

    -- Parse response
    let Just listResult = decode @(ListRecordsResponse BskyPost) (simpleBody listResponse)
    liftIO $ do
      cursor listResult `shouldBe` Nothing
      records listResult `shouldBe` []
