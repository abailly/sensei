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

    it "parses -n as 'Note' flow type given flows list contains 'Refactoring'" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["-n"] `shouldBe` Right (RecordOptions Note)

    it "parses '-U' as user profile query" $ do
      runOptionsParser Nothing ["-U"] `shouldBe` Right (UserOptions GetProfile)

    it "parses '-U -c config' as user profile upload" $ do
      runOptionsParser Nothing ["-U", "-c", "config" ] `shouldBe` Right (UserOptions (SetProfile "config"))

    it "parses '-v' as versions display" $ do
      runOptionsParser Nothing ["-v" ] `shouldBe` Right (UserOptions GetVersions)

    it "parses '-S -10m' as 10 minutes shift" $ do
      runOptionsParser Nothing ["-S", "-10m" ] `shouldBe` Right (UserOptions $ ShiftTimestamp (Minutes (-10)))

    it "parses '-Q latest' as retrieval of latest flow" $ do
      runOptionsParser Nothing ["-Q", "latest" ] `shouldBe` Right (UserOptions $ GetFlow Latest)

    it "parses '-N --search foo' as search query" $ do
      runOptionsParser Nothing ["-N", "--search", "foo", "-f", "section" ] `shouldBe` Right (NotesOptions (QuerySearch "foo") Section)
      runOptionsParser Nothing ["-N", "-s", "foo", "-f", "section" ] `shouldBe` Right (NotesOptions (QuerySearch "foo") Section)
