{-# LANGUAGE OverloadedStrings #-}

module Sensei.FlowAPISpec where

import Data.Aeson
import Data.Time
import Sensei.API
import Sensei.TestHelper
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

spec :: Spec
spec = withApp $
  describe "Flows API" $ do
    it "POST /flows/<user>/Other with Flow body register start of a flow event" $ do
      let flow = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
      postJSON "/flows/arnaud/Other" flow
        `shouldRespondWith` 200

    it "GET /flows/<user> retrieves all Flows ungrouped" $ do
      let flow1 = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = FlowState "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Meeting" flow2

      let expectedGroups =
            [ Leaf
                [ FlowView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50000) (TimeOfDay 18 30 0)) Other,
                  FlowView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (FlowType "Meeting")
                ]
            ]

      getJSON "/flows/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expectedGroups)

    it "GET /flows/<user>?group=Day retrieves all Flows grouped by Day" $ do
      let flow1 = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = FlowState "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Meeting" flow2

      let expectedGroups =
            [ GroupLevel
                Day
                (LocalTime (toEnum 50000) (TimeOfDay 1 0 0))
                (Leaf [FlowView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50000) (TimeOfDay 18 30 0)) Other]),
              GroupLevel
                Day
                (LocalTime (toEnum 50001) (TimeOfDay 1 0 0))
                (Leaf [FlowView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50001) (TimeOfDay 18 30 0)) (FlowType "Meeting")])
            ]

      getJSON "/flows/arnaud?group=Day"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expectedGroups)

    it "GET /flows/<user>/<day>/notes retrieves Notes for given day" $ do
      let flow1 = FlowState "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = FlowNote "arnaud" (UTCTime (toEnum 50001) 0) "some/directory" "some note"
          expectedNotes = [NoteView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) "some note"]

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Note" flow2

      getJSON "/flows/arnaud/1995-10-11/notes"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expectedNotes)

    it "GET /flows/<user>/<day>/commands retrieves commands run for given day" $ do
      let cmd1 = Trace (UTCTime (toEnum 50000) 0) "some/directory" "foo" ["bar"] 0 10 1
          cmd2 = Trace (UTCTime (toEnum 50000) 1000) "other/directory" "git" ["bar"] 0 100 1

          expected = [CommandView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) "foo" 10, CommandView (LocalTime (toEnum 50000) (TimeOfDay 1 16 40)) "git" 100]

      postJSON_ "/trace" cmd1
      postJSON_ "/trace" cmd2

      getJSON "/flows/arnaud/1995-10-10/commands"
        `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals $ encode expected)
