module Sensei.Server.AuthSpec where

import Control.Exception (ErrorCall)
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Proxy (Proxy (..))
import Sensei.Server.Auth.Types (JWK, SerializedToken (..), createKeys, createToken, getKey)
import Sensei.TestHelper (shouldNotThrow, withTempDir)
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

  it "can create token given keys exist in given directory" $ do
    withTempDir $ \dir -> do
      createKeys dir

      SerializedToken bsToken <- createToken dir

      take 2 (B64.decode <$> BS.split (fromIntegral $ ord '.') bsToken)
        `shouldBe` [Right "{\"alg\":\"PS512\"}", Right "{\"dat\":{\"auOrgID\":1,\"auID\":1}}"]
