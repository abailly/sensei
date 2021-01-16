{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.LogSpec where

import Network.HTTP.Link
import Network.URI.Extra()
import Data.Time
import Sensei.DB.Model
import Sensei.Builder
import Sensei.TestHelper
import Data.Text.Encoding(encodeUtf8)
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck


spec :: Spec
spec = withApp app $
  describe "Log API" $ do

    it "GET /api/log/<user> returns all events in reverse timestamp order" $ do
      events <- liftIO $ generate (sequence $ map (generateEvent (UTCTime (toEnum 50000) 0)) [1 :: Integer ..20])
      mapM_ postEvent_ events

      getJSON "/api/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals $ reverse events)

    it "GET /api/log/<user>?page=2 returns next 50 events and link to previous page" $ do
      events <- liftIO $ generate (sequence $ map (generateEvent (UTCTime (toEnum 50000) 0)) [1 :: Integer .. 100])
      mapM_ postEvent_ events

      getJSON "/api/log/arnaud?page=2"
        `shouldRespondWith` ResponseMatcher 200 ["Link" <:> encodeUtf8 (writeLinkHeader [ Link "/api/log/arnaud?page=1" [(Rel, "prev"), (Other "page", "1")]])] (jsonBodyEquals $ drop 50 $ reverse events)

    it "GET /api/log/<user> returns 1 page and link to next page given there's more events" $ do
      events <- liftIO $ generate (sequence $ map (generateEvent (UTCTime (toEnum 50000) 0)) [1 :: Integer .. 100])
      mapM_ postEvent_ events

      getJSON "/api/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [ "Link" <:> encodeUtf8 (writeLinkHeader [ Link "/api/log/arnaud?page=2" [(Rel, "next"), (Other "page", "2")]])] (jsonBodyEquals $ take 50 $ reverse events)
