module Sensei.Server.AuthSpec where

import Control.Exception (ErrorCall)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Sensei.Server.Auth.Types (JWK, createKeys, getKey)
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
