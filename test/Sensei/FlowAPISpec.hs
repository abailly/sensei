{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.FlowAPISpec where

import Data.Maybe (catMaybes)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Link (writeLinkHeader)
import Sensei.API
import Sensei.TestHelper
import Sensei.Time
import Test.Hspec

spec :: Spec
spec = withApp app $
  describe "Flows API" $ do
    it "POST /flows/<user>/Other with Flow body register start of a flow event" $ do
      let flow = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
      postJSON "/flows/arnaud/Other" flow
        `shouldRespondWith` 200

    it "GET /flows/<user> retrieves all Flows ungrouped" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow (FlowType "Meeting") "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Meeting" flow2

      let expectedGroups =
            [ Leaf
                [ FlowView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50000) (TimeOfDay 18 30 0)) Other,
                  FlowView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) (FlowType "Meeting")
                ]
            ]

      getJSON "/flows/arnaud"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals expectedGroups)

    it "GET /flows/<user>?group=Day retrieves all Flows grouped by Day" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow (FlowType "Meeting") "arnaud" (UTCTime (toEnum 50001) 0) "some/directory"
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
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals expectedGroups)

    it "GET /flows/<user>/<day>/notes retrieves Notes for given day with link headers" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = NoteFlow "arnaud" (UTCTime (toEnum 50001) 0) "some/directory" "some note"
          expectedNotes = [NoteView (LocalTime (toEnum 50001) (TimeOfDay 1 0 0)) "some note"]

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Note" flow2

      getJSON "/flows/arnaud/1995-10-11/notes"
        `shouldRespondWith` ResponseMatcher
          200
          [ "Link"
              <:> encodeUtf8
                ( writeLinkHeader $
                    catMaybes
                      [ nextDayLink "arnaud" (Just $ toEnum 50001),
                        previousDayLink "arnaud" (Just $ toEnum 50001)
                      ]
                )
          ]
          (jsonBodyEquals expectedNotes)

    it "GET /flows/<user>/<day>/commands retrieves commands run for given day" $ do
      let cmd1 = Trace "arnaud" (UTCTime (toEnum 50000) 0) "some/directory" "foo" ["bar"] 0 10
          cmd2 = Trace "arnaud" (UTCTime (toEnum 50000) 1000) "other/directory" "git" ["bar"] 0 100

          expected =
            [ CommandView (LocalTime (toEnum 50000) (TimeOfDay 1 0 0)) "foo" 10,
              CommandView (LocalTime (toEnum 50000) (TimeOfDay 1 16 40)) "git" 100
            ]

      postJSON_ "/trace" cmd1
      postJSON_ "/trace" cmd2

      getJSON "/flows/arnaud/1995-10-10/commands"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals expected)

    it "GET /flows/<user>/day/summary returns a summary of flows and traces for given day" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow (FlowType "Learning") "arnaud" (UTCTime (toEnum 50000) 1000) "some/directory"
          cmd1 = Trace "arnaud" (UTCTime (toEnum 50000) 0) "some/directory" "foo" ["bar"] 0 10
          cmd2 = Trace "arnaud" (UTCTime (toEnum 50000) 1000) "other/directory" "git" ["bar"] 0 100

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Learning" flow2
      postJSON_ "/trace" cmd1
      postJSON_ "/trace" cmd2

      let expected =
            FlowSummary
              { summaryPeriod = (toEnum 50000, toEnum 50000),
                summaryFlows = [(FlowType "Learning", 0), (Other, 1000)],
                summaryCommands = [("foo", 10), ("git", 100)]
              }

      getJSON "/flows/arnaud/1995-10-10/summary"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals expected)

    it "PATCH /flows/<user>/latest/timestamp updates latest flow's timestamp" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow Other "arnaud" (UTCTime (toEnum 50000) 1000) "some/directory"
          trace = Trace "arnaud" (UTCTime (toEnum 50000) 2000) "other/directory" "git" ["bar"] 0 100

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Other" flow2
      postJSON_ "/trace" trace

      let expected = flow1 {_flowTimestamp = (UTCTime (toEnum 50000) 400)}
          timeshift :: TimeDifference = Minutes (-10)

      patchJSON "/flows/arnaud/latest/timestamp" timeshift
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals expected)

    it "GET /flows/<user>/latest retrieves latest flow" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow Other "arnaud" (UTCTime (toEnum 50000) 1000) "some/directory"

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Other" flow2

      getJSON "/flows/arnaud/latest"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals flow2)

    it "GET /flows/<user>/2 retrieves flow 2 steps back" $ do
      let flow1 = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"
          flow2 = Flow Other "arnaud" (UTCTime (toEnum 50000) 1000) "some/directory"
          flow3 = Flow Other "arnaud" (UTCTime (toEnum 50000) 2000) "some/directory"

      postJSON_ "/flows/arnaud/Other" flow1
      postJSON_ "/flows/arnaud/Other" flow2
      postJSON_ "/flows/arnaud/Other" flow3

      getJSON "/flows/arnaud/2"
        `shouldRespondWith` ResponseMatcher 200 [] (jsonBodyEquals flow1)
