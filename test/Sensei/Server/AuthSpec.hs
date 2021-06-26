{-# LANGUAGE OverloadedStrings #-}

module Sensei.Server.AuthSpec where

import Control.Exception (ErrorCall)
import Data.Aeson (decode)
import Data.Maybe(fromJust)
import Control.Monad.Trans(liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Proxy (Proxy (..))
import Sensei.Server.Auth.Types (Credentials(..), JWK, SerializedToken (..), createKeys, createToken, getKey, setPassword)
import Sensei.TestHelper (matchHeaders,request, defaultHeaders,getSessionCookie, MatchHeader(..), putJSON_, postJSON, withApp, app,shouldRespondWith,shouldNotThrow, withTempDir)
import System.FilePath ((</>))
import Sensei.API(UserProfile(..), defaultProfile)
import Test.Hspec

spec :: Spec
spec = describe "Authentication Operations" $ do
  it "can create pair of keys in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      jwk <- LBS.readFile (dir </> "sensei.jwk")

      (decode jwk :: Maybe JWK) `shouldNotBe` Nothing
      getKey (dir </> "sensei.jwk") `shouldNotThrow` (Proxy :: Proxy ErrorCall)

  it "can create token given keys exist in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      SerializedToken bsToken <- createToken dir

      take 2 (B64.decode <$> BS.split (fromIntegral $ ord '.') bsToken)
        `shouldBe` [Right "{\"alg\":\"PS512\"}", Right "{\"dat\":{\"auOrgID\":1,\"auID\":1}}"]

  it "can update profile with hashed password given cleartext password" $ do
    let profile = defaultProfile

    newProfile <- setPassword profile "password" 

    userPassword newProfile `shouldNotBe` userPassword profile


  withApp app $
    describe "Authentication API" $ do
      it "POST /login returns 200 given user authenticates with valid password" $ do
        profile <- liftIO $ setPassword defaultProfile "password"

        putJSON_ "/api/users/arnaud" profile

        let credentials = Credentials (userName profile) "password"
        postJSON "/login" credentials `shouldRespondWith` 200 { matchHeaders = [ has2Cookies ] }

      it "POST /api/flows/<user> returns 200 given user authenticates with cookies" $ do
        profile <- liftIO $ setPassword defaultProfile "password"
        putJSON_ "/api/users/arnaud" profile
        let credentials = Credentials (userName profile) "password"
        resp <- postJSON "/login" credentials
      
        let authCookie = fromJust $ getSessionCookie resp
            headers = filter ((/= "Authorization") . fst) defaultHeaders <> [("Cookie" ,"JWT-Cookie=" <> authCookie)]

        request "GET" "/api/flows/arnaud" headers mempty `shouldRespondWith` 200

        
      it "POST /login returns 401 given user authenticates with invalid password" $ do
        profile <- liftIO $ setPassword defaultProfile "password"

        putJSON_ "/api/users/arnaud" profile

        let credentials = Credentials (userName profile) "wrong password"
        postJSON "/login" credentials `shouldRespondWith` 401

has2Cookies :: MatchHeader
has2Cookies = MatchHeader $ \ hdrs _ ->
  if length (filter ((== "Set-Cookie") . fst) hdrs) == 2
  then Nothing
  else Just $ "Expected 2 Set-Cookie headers, got: " <> show hdrs
