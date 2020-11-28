{-# LANGUAGE OverloadedStrings #-}
module Sensei.UISpec where

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai
import Sensei.Server.UI

embeddedApp :: IO Application
embeddedApp = pure $ embeddedFiles $ [("index.html", "<html></html>")]

spec :: Spec
spec =
  with embeddedApp $ describe "Embedded UI Files" $ do
  it "GET /index.html returns file content and 200" $ do
    get "/index.html" `shouldRespondWith` 200
  it "GET /foo.html returns 404" $ do
    get "/foo.html" `shouldRespondWith` 404
