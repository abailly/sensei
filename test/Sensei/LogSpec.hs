{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.LogSpec where

import Data.Aeson
import Data.Time
import Sensei.API
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

    it "GET /logs/<user> returns all events registered" $ do
      f <- liftIO $ generate (generateFlow (UTCTime (toEnum 50000) 0) 1)
      postFlow_ f

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode [F f])

    it "GET /logs/<user> returns all events registered" $ do
      t <- liftIO $ generate (generateTrace (UTCTime (toEnum 50000) 0) 1)
      postTrace_ t

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode [T t])

    it "GET /logs/<user> returns all events in reverse timestamp order" $ do
      flows <- liftIO $ generate (sequence $ map (generateFlow (UTCTime (toEnum 50000) 0)) [1 :: Integer ..20])
      mapM_ postFlow_ flows

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode $ reverse $ map F flows)

    it "GET /logs/<user> returns at most 50 events" $ do
      flows <- liftIO $ generate (sequence $ map (generateFlow (UTCTime (toEnum 50000) 0)) [1 :: Integer .. 100])
      mapM_ postFlow_ flows

      getJSON "/log/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode $ take 50 $ reverse $ map F flows)
