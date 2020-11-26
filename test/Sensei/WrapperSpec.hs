{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.WrapperSpec where

import qualified Data.Map as Map
import Data.Time(UTCTime(..))
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Sensei.API
import Sensei.Client hiding (send)
import Sensei.TestHelper
import Sensei.Wrapper
import Sensei.WaiTestHelper
import System.Exit
import Test.Hspec
import Test.Hspec.Wai hiding (request)

io :: WrapperIO (WaiSession ())
io = WrapperIO {..}
  where
    runProcess _ _ = pure ExitSuccess
    getCurrentTime = pure $ UTCTime (toEnum 50000) 0
    send (ClientMonad a) = a

spec :: Spec
spec =
  withApp app $
    describe "Program wrapper" $ do
      it "records execution trace of wrapped program and returns program's exit code" $ do
        res <- wrapProg io "git" ["status"] "somedir"
        res `isExpectedToBe` ExitSuccess

      it "selects program to run from User Profile" $ do
        send io $ setUserProfileC "alice" defaultProfile {userCommands = Just $ Map.fromList [("foo", "/usr/bin/foo")]}
        res <- tryWrapProg io "alice" "foo" [] "somedir"
        res `isExpectedToBe` Right ExitSuccess

      it "return error when called with a non-mapped alias" $ do
        send io $ setUserProfileC "alice" defaultProfile {userCommands = Just $ Map.fromList [("foo", "/usr/bin/foo")]}
        res <- tryWrapProg io "alice" "bar" [] "somedir"
        res `isExpectedToBe` Left "Don't know how to handle program 'bar'"
