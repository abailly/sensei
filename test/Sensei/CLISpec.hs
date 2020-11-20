{-# LANGUAGE OverloadedStrings #-}
module Sensei.CLISpec where

import Test.Hspec
import Sensei.CLI
import Data.Either (isLeft)
import Sensei.API

spec :: Spec
spec = describe "Command-Line Interface" $ do
  describe "Options parser" $ do
    it "parses -e as 'Experimenting' flow type given flows is Nothing" $ do
      runOptionsParser Nothing ["-e"] `shouldBe` Right (RecordOptions (FlowType "Experimenting"))

    it "parses -g as an error given flows is Nothing" $ do
      runOptionsParser Nothing ["-g"] `shouldSatisfy` isLeft

    it "parses -r as 'Refactoring' flow type given flows list contains 'Refactoring'" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["-r"] `shouldBe` Right (RecordOptions (FlowType "Refactoring"))
