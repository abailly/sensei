{-# LANGUAGE OverloadedStrings #-}

module Sensei.ProjectSpec where

import qualified Data.Map as Map
import Sensei.API
import Test.Hspec

spec :: Spec
spec = describe "Project Selector" $ do
    it "returns last non-empty segment of given path given no regex matches" $ do
        selectProject mempty "foo/bar/baz" `shouldBe` "baz"
        selectProject mempty "foo/bar/baz/" `shouldBe` "baz"
        selectProject mempty "/" `shouldBe` ""
        selectProject mempty "" `shouldBe` ""

    it "returns matching project name given path given matches regex" $ do
        selectProject (Map.fromList [(".*quux", "my project")]) "foo/bar/quux"
            `shouldBe` "my project"

    it "returns first matching project name given path given matches multiple regexes" $ do
        selectProject (Map.fromList [(".*quux", "my project"), (".*bar.*", "other project")]) "foo/bar/quux"
            `shouldBe` "my project"
