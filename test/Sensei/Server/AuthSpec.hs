{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Server.AuthSpec where

import Control.Exception (ErrorCall)
import Control.Monad.Trans (liftIO)
import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.Functor (void)
import Data.Proxy (Proxy (..))
import Preface.Codec (Encoded (..))
import Sensei.API (UserProfile (..), defaultProfile)
import Sensei.Server (
  Credentials (..),
  Error,
  JWK,
  SerializedToken (..),
  SignedJWT,
  createKeys,
  createToken,
  decodeCompact,
  encryptPassword,
  getKey,
  getPublicKey,
  setPassword,
 )
import Sensei.TestHelper (
  MatchHeader (..),
  app,
  bodySatisfies,
  clearCookies,
  defaultHeaders,
  getJSON,
  jsonBodyMatches,
  matchBody,
  matchHeaders,
  postJSON,
  postJSON_,
  putJSON_,
  request,
  shouldNotThrow,
  shouldRespondWith,
  withApp,
  withRootPassword,
  withRootUser,
  withTempDir,
  withoutRootUser,
 )
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Authentication Operations" $ do
  it "can create pair of keys in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      jwk <- LBS.readFile (dir </> "sensei.jwk")

      (decode jwk :: Maybe JWK) `shouldNotBe` Nothing
      getKey (dir </> "sensei.jwk") `shouldNotThrow` (Proxy :: Proxy ErrorCall)

  it "can retrieve public key from private key in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      void $ getPublicKey dir

  it "can create token given keys exist in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      SerializedToken bsToken <- createToken dir

      -- NOTE: seems like JWT's base64 components are not properly padded which breaks
      -- strict Base64.decode function
      take 2 (B64.decodeLenient <$> BS.split (fromIntegral $ ord '.') bsToken)
        `shouldBe` [ "{\"alg\":\"PS512\"}"
                   , LBS.toStrict $
                      encode $
                        object
                          [ "dat"
                              .= object
                                [ "auOrgID" .= (1 :: Int)
                                , "auID" .= ("" :: String)
                                ]
                          ]
                   ]

  it "can update profile with hashed password given cleartext password" $ do
    let profile = defaultProfile

    newProfile <- setPassword profile "password"

    userPassword newProfile `shouldNotBe` userPassword profile

  it "can output hashed password given cleartext password" $ do
    (Encoded salt, _) <- encryptPassword "password"

    BS.length salt `shouldBe` 16

  withApp (withoutRootUser app) $
    it "does not initialise root user given it's not provided" $ do
      getJSON "/api/users/arnaud" `shouldRespondWith` 404

  withApp (withRootPassword "duck" $ withRootUser "arnaud" app) $
    it "initialises root user with a password" $ do
      let credentials = Credentials "arnaud" "password"
      postJSON "/login" credentials
        `shouldRespondWith` 401

  withApp app $
    describe "Authentication API" $ do
      it "POST /login returns 200 with user profile given user authenticates with valid password" $ do
        profile <- liftIO $ setPassword defaultProfile "password"

        putJSON_ "/api/users/arnaud" profile

        let credentials = Credentials (userName profile) "password"
        postJSON "/login" credentials
          `shouldRespondWith` 200
            { matchHeaders = [hasCookies 2]
            , matchBody = jsonBodyMatches (\p -> p{userId = ""} == profile)
            }

      it "GET /logout returns 200 and clears cookies" $ do
        profile <- liftIO $ setPassword defaultProfile "password"
        putJSON_ "/api/users/arnaud" profile
        postJSON_ "/login" (Credentials (userName profile) "password")

        getJSON "/logout"
          `shouldRespondWith` 204
            { matchHeaders = [hasCookies 4]
            }

      it "GET /api/users/<user>/token returns fresh token given user is authenticated" $ do
        getJSON "/api/users/arnaud/token" `shouldRespondWith` 200{matchBody = bodySatisfies isSerializedToken}

      it "POST /login returns 401 given user authenticates with invalid password" $ do
        profile <- liftIO $ setPassword defaultProfile "password"

        putJSON_ "/api/users/arnaud" profile

        let credentials = Credentials (userName profile) "wrong password"
        postJSON "/login" credentials `shouldRespondWith` 401

      it "POST /api/flows/<user> returns 200 given user authenticates with JWT contained in cookie" $ do
        profile <- liftIO $ setPassword defaultProfile "password"
        putJSON_ "/api/users/arnaud" profile
        let credentials = Credentials (userName profile) "password"
        postJSON_ "/login" credentials

        headers <- filter ((/= "Authorization") . fst) <$> defaultHeaders

        request "GET" "/api/flows/arnaud" headers mempty `shouldRespondWith` 200

      it "POST /api/<XXX> returns 401 given user agent fails to provide Authorization header or JWT-Cookie" $ do
        profile <- liftIO $ setPassword defaultProfile "password"
        putJSON_ "/api/users/arnaud" profile
        let credentials = Credentials (userName profile) "password"
        postJSON_ "/login" credentials

        headers <- filter ((/= "Authorization") . fst) <$> defaultHeaders
        clearCookies

        request "GET" "/api/flows/arnaud" headers mempty `shouldRespondWith` 401

hasCookies :: Int -> MatchHeader
hasCookies n = MatchHeader $ \hdrs _ ->
  if length (filter ((== "Set-Cookie") . fst) hdrs) == n
    then Nothing
    else Just $ "Expected 2 Set-Cookie headers, got: " <> show hdrs

isSerializedToken :: BS.ByteString -> Bool
isSerializedToken bytes =
  case eitherDecode (LBS.fromStrict bytes) of
    Right st -> either (const False) (const True) $ decodeCompact @SignedJWT @Error (LBS.fromStrict $ unToken st)
    Left _ -> False
