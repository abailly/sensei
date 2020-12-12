{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.LogSpec where

import Data.Aeson
import Data.Time
import Sensei.DB.Model
import Sensei.Builder
import Sensei.TestHelper
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import Test.Hspec.Wai.Matcher


spec :: Spec
spec = withApp app $
  describe "Log API" $ do

    it "GET /logs/<user> returns all events in reverse timestamp order" $ do
      events <- liftIO $ generate (sequence $ map (generateEvent (UTCTime (toEnum 50000) 0)) [1 :: Integer ..20])
      mapM_ postEvent_ events

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode $ reverse events)

    it "GET /logs/<user> returns at most 50 events" $ do
      events <- liftIO $ generate (sequence $ map (generateEvent (UTCTime (toEnum 50000) 0)) [1 :: Integer .. 100])
      mapM_ postEvent_ events

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode $ take 50 $ reverse events)
