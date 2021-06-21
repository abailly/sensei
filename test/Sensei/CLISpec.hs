{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sensei.CLISpec where

import Data.Either (isLeft)
import Sensei.API
import Sensei.CLI
import Test.Hspec

spec :: Spec
spec = describe "Command-Line Interface" $ do
  describe "Options parser" $ do
    it "parses 'record -e' as 'Experimenting' flow type given flows is Nothing" $ do
      runOptionsParser Nothing ["record", "-e"] `shouldBe` Right (RecordOptions (FlowType "Experimenting"))

    it "parses 'record -g' as an error given flows is Nothing" $ do
      runOptionsParser Nothing ["record", "-g"] `shouldSatisfy` isLeft

    it "parses 'record -r' as 'Refactoring' flow type given flows list contains 'Refactoring'" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["record", "-r"] `shouldBe` Right (RecordOptions (FlowType "Refactoring"))

    it "parses 'record -n' as 'Note' flow type" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["record", "-n"] `shouldBe` Right (RecordOptions Note)

    it "parses 'user -U' as user profile query" $ do
      runOptionsParser Nothing ["user", "-U"] `shouldBe` Right (UserOptions GetProfile)

    it "parses 'user -U -c config' as user profile upload" $ do
      runOptionsParser Nothing ["user", "-U", "-c", "config"] `shouldBe` Right (UserOptions (SetProfile "config"))

    it "parses 'user -v' as versions display" $ do
      runOptionsParser Nothing ["user", "-v"] `shouldBe` Right (UserOptions GetVersions)

    it "parses 'user -S -10m' as 10 minutes shift" $ do
      runOptionsParser Nothing ["user", "-S", "-10m"] `shouldBe` Right (UserOptions $ ShiftTimestamp (Minutes (-10)))

    it "parses 'user -Q latest' as retrieval of latest flow" $ do
      runOptionsParser Nothing ["user", "-Q", "latest"] `shouldBe` Right (UserOptions $ GetFlow Latest)

    it "parses 'notes -N --search foo' as search query" $ do
      runOptionsParser Nothing ["notes", "-N", "--search", "foo", "-f", "section"] `shouldBe` Right (NotesOptions (QuerySearch "foo") Section)
      runOptionsParser Nothing ["notes", "-N", "-s", "foo", "-f", "section"] `shouldBe` Right (NotesOptions (QuerySearch "foo") Section)

    it "parses 'query -s' as a summary with default date" $ do
      runOptionsParser Nothing ["query", "-s"] `shouldSatisfy` \case
        (Right (QueryOptions _ True [])) -> True
        _ -> False
