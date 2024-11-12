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
      runOptionsParser Nothing ["record", "-e"] `shouldBe` Right (RecordOptions $ SingleFlow (FlowType "Experimenting"))

    it "parses 'record -g' as an error given flows is Nothing" $ do
      runOptionsParser Nothing ["record", "-g"] `shouldSatisfy` isLeft

    it "parses 'record -r' as 'Refactoring' flow type given flows list contains 'Refactoring'" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["record", "-r"] `shouldBe` Right (RecordOptions $ SingleFlow (FlowType "Refactoring"))

    it "parses 'record -n' as 'Note' flow type" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["record", "-n"] `shouldBe` Right (RecordOptions $ SingleFlow Note)

    it "parses 'record --from-file <somefile>' as recording events from raw json file" $ do
      runOptionsParser (Just [FlowType "Refactoring"]) ["record", "--from-file", "somefile.json"]
        `shouldBe` Right (RecordOptions $ FromFile "somefile.json")

    it "parses 'user -U' as user profile query" $ do
      runOptionsParser Nothing ["user", "-U"] `shouldBe` Right (UserOptions GetProfile)

    it "parses 'user -U -c config' as user profile upload" $ do
      runOptionsParser Nothing ["user", "-U", "-c", "config"] `shouldBe` Right (UserOptions (SetProfile "config"))

    it "parses 'user -v' as versions display" $ do
      runOptionsParser Nothing ["user", "-v"] `shouldBe` Right (UserOptions GetVersions)

    it "parses 'query -S -10m' as 10 minutes shift" $ do
      runOptionsParser Nothing ["query", "-S", "-10m"] `shouldBe` Right (QueryOptions $ ShiftTimestamp (Minutes (-10)))

    it "parses 'query -F latest' as retrieval of latest flow" $ do
      runOptionsParser Nothing ["query", "-F", "latest"] `shouldBe` Right (QueryOptions $ GetFlow Latest)

    it "parses 'notes -N --search foo' as search query" $ do
      runOptionsParser Nothing ["notes", "-N", "--search", "foo", "-f", "section"] `shouldBe` Right (NotesOptions (NotesQuery (QuerySearch "foo") Section))
      runOptionsParser Nothing ["notes", "-N", "-s", "foo", "-f", "section"] `shouldBe` Right (NotesOptions (NotesQuery (QuerySearch "foo") Section))

    it "parses 'query -s' as a summary with default date" $ do
      runOptionsParser Nothing ["query", "-s"] `shouldSatisfy` \case
        (Right (QueryOptions (FlowQuery _ True []))) -> True
        _ -> False

    it "parses 'query -l' as a querying all logs" $ do
      runOptionsParser Nothing ["query", "-l"] `shouldBe` Right (QueryOptions GetAllLogs)

    it "parses 'auth --create-keys' as creation of new key pair" $ do
      runOptionsParser Nothing ["auth", "--create-keys"] `shouldBe` Right (AuthOptions CreateKeys)

    it "parses 'auth --public-key' as extraction of public key" $ do
      runOptionsParser Nothing ["auth", "--public-key"] `shouldBe` Right (AuthOptions PublicKey)

    it "parses 'auth --set-password' as password creation" $ do
      runOptionsParser Nothing ["auth", "--set-password"] `shouldBe` Right (AuthOptions SetPassword)

    it "parses 'auth --encrypt-password' as password encryption" $ do
      runOptionsParser Nothing ["auth", "--encrypt-password"] `shouldBe` Right (AuthOptions EncryptPassword)

    it "parses 'auth --get-token' as token retrieval" $ do
      runOptionsParser Nothing ["auth", "--get-token"] `shouldBe` Right (AuthOptions GetToken)

    it "parses 'command -- foo bar -x' as executing command 'foo' with arguments" $ do
      runOptionsParser Nothing ["command", "--", "foo", "bar", "-x"]
        `shouldBe` Right (CommandOptions $ Command "foo" ["bar", "-x"])

    it "fail to parse 'command foo bar -x' as it contains interpreted option" $ do
      runOptionsParser Nothing ["command", "foo", "bar", "-x"]
        `shouldSatisfy` isLeft

    describe "Goal Options" $ do
      it "parses 'goal -g foo' as goal option" $ do
        runOptionsParser Nothing ["goal", "-g", "foo"]
          `shouldBe` Right (GoalOptions $ UpdateGraph $ goal "foo")

      it "parses 'goal -p' as goal push option" $ do
        runOptionsParser Nothing ["goal", "-p"]
          `shouldBe` Right (GoalOptions $ UpdateGraph push)

      it "parses 'goal -P' as goal pop option" $ do
        runOptionsParser Nothing ["goal", "-P"]
          `shouldBe` Right (GoalOptions $ UpdateGraph pop)

      it "parses 'goal -s' as goal shift option" $ do
        runOptionsParser Nothing ["goal", "-s"]
          `shouldBe` Right (GoalOptions $ UpdateGraph shift)

      it "parses 'goal -d' as goal done option" $ do
        runOptionsParser Nothing ["goal", "-d"]
          `shouldBe` Right (GoalOptions $ UpdateGraph done)

      it "parses 'goal -a \"some goal\"' as goal add option" $ do
        runOptionsParser Nothing ["goal", "-a", "a goal"]
          `shouldBe` Right (GoalOptions $ UpdateGraph $ add "a goal")

      it "parses 'goal' as get goal graph option" $ do
        runOptionsParser Nothing ["goal"]
          `shouldBe` Right (GoalOptions GetGraph)

      it "parses 'goal -l goal1 goal2' as link goal option" $ do
        runOptionsParser Nothing ["goal", "-l", "goal1", "goal2"]
          `shouldBe` Right (GoalOptions $ UpdateGraph $ link "goal1" "goal2")

      it "parses 'goal -r foo' as remove option" $ do
        runOptionsParser Nothing ["goal", "-r", "foo"]
          `shouldBe` Right (GoalOptions $ UpdateGraph $ remove "foo")
