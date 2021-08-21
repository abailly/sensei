{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.FlowAPISpec where

import Control.Lens ((.~))
import Data.Function ((&))
import Data.Maybe (catMaybes, fromJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Lens (modL)
import Sensei.API
import Sensei.Builder
import Sensei.Server hiding (Other)
import Sensei.TestHelper
import Sensei.Time
import Test.Hspec

spec :: Spec
spec = withApp app $
  describe "Flows API" $ do
    it "POST /api/log with Flow body register start of a flow event" $ do
      postFlow anOtherFlow
        `shouldRespondWith` 200

    it "GET /api/flows/<user> retrieves all Flows ungrouped" $ do
      let flow1 = anOtherFlow
          flow2 =
            anOtherFlow
              & (flowType .~ FlowType "Meeting")
                . (flowTimestamp .~ UTCTime (succ aDay) 0)

      postFlow_ flow1
      postFlow_ flow2

      let expectedGroups =
            [ Leaf $
                FlowView
                  { flowStart = LocalTime aDay oneAM,
                    flowEnd = LocalTime aDay sixThirtyPM,
                    viewType = Other,
                    flowProject = "directory"
                  },
              Leaf $
                FlowView
                  { flowStart = LocalTime (succ aDay) oneAM,
                    flowEnd = LocalTime (succ aDay) oneAM,
                    viewType = FlowType "Meeting",
                    flowProject = "directory"
                  }
            ]

      getJSON "/api/flows/arnaud" `shouldRespondJSONBody` expectedGroups

    it "GET /api/flows/<user>?group=Day retrieves all Flows grouped by Day" $ do
      let flow1 = anOtherFlow
          flow3 = anOtherFlow & later 1000 seconds
          flow2 =
            anOtherFlow
              & (flowType .~ FlowType "Meeting")
                . (flowTimestamp .~ UTCTime (succ aDay) 0)
      postFlow_ flow1
      postFlow_ flow3
      postFlow_ flow2

      let expectedGroups =
            [ GroupLevel
                Day
                (LocalTime aDay oneAM)
                [ Leaf $
                    FlowView
                      { flowStart = LocalTime aDay oneAM,
                        flowEnd = LocalTime aDay (TimeOfDay 1 16 40),
                        viewType = Other,
                        flowProject = "directory"
                      },
                  Leaf $
                    FlowView
                      { flowStart = LocalTime aDay (TimeOfDay 1 16 40),
                        flowEnd = LocalTime aDay sixThirtyPM,
                        viewType = Other,
                        flowProject = "directory"
                      }
                ],
              GroupLevel
                Day
                (LocalTime (succ aDay) oneAM)
                [ Leaf $
                    FlowView
                      { flowStart = LocalTime (succ aDay) oneAM,
                        flowEnd = LocalTime (succ aDay) sixThirtyPM,
                        viewType = FlowType "Meeting",
                        flowProject = "directory"
                      }
                ]
            ]

      getJSON "/api/flows/arnaud?group=Day" `shouldRespondJSONBody` expectedGroups

    it "GET /api/flows/<user>/<day>/notes retrieves Notes for given day with link headers" $ do
      let flow1 = anOtherFlow
          flow2 = NoteFlow "arnaud" (UTCTime (succ aDay) 0) "some/directory" "some note"
          expectedNotes = [NoteView (LocalTime (succ aDay) oneAM) "some note" "directory" []]

      postFlow_ flow1
      postNote_ flow2

      getJSON "/api/flows/arnaud/1995-10-11/notes"
        `shouldRespondWith` ResponseMatcher
          200
          [ "Link"
              <:> encodeUtf8
                ( writeLinkHeader $
                    catMaybes
                      [ nextDayLink "arnaud" (Just $ succ aDay),
                        previousDayLink "arnaud" (Just $ succ aDay)
                      ]
                )
          ]
          (jsonBodyEquals expectedNotes)

    it "GET /api/flows/<user>/<day>/commands retrieves commands run for given day" $ do
      let cmd1 = Trace "arnaud" (UTCTime aDay 0) "some/directory" "foo" ["bar"] 0 10
          cmd2 = Trace "arnaud" (UTCTime aDay 1000) "other/directory" "git" ["bar"] 0 100

          expected =
            [ CommandView (LocalTime aDay oneAM) "foo" 10 "directory",
              CommandView (LocalTime aDay (TimeOfDay 1 16 40)) "git" 100 "directory"
            ]

      postTrace_ cmd1
      postTrace_ cmd2

      getJSON "/api/flows/arnaud/1995-10-10/commands"
        `shouldRespondJSONBody` expected

    it "GET /api/flows/<user>/summary returns a summary of flows and traces for given period" $ do
      let flow1 = anOtherFlow
          flow2 = Flow (FlowType "Learning") "arnaud" (UTCTime aDay 1000) "some/directory"
          flow3 = flow2 & later 1 month
          cmd1 = Trace "arnaud" (UTCTime aDay 0) "some/directory" "foo" ["bar"] 0 10
          cmd2 = Trace "arnaud" (UTCTime aDay 1000) "other/directory" "git" ["bar"] 0 100

      postFlow_ flow1
      postFlow_ flow2
      postFlow_ flow3
      postTrace_ cmd1
      postTrace_ cmd2

      let expected =
            FlowSummary
              { summaryPeriod = (startPeriod, endPeriod),
                summaryFlows = [(FlowType "Learning", 62000), (Other, 1000)],
                summaryCommands = [("foo", 10), ("git", 100)]
              }
          startPeriod = LocalTime aDay midnight
          endPeriod = startPeriod & modL month (+ 1)

      getJSON "/api/flows/arnaud/summary?from=1995-10-10&to=1995-11-10"
        `shouldRespondJSONBody` expected

    it "GET /api/flows/<user>/summary returns link to next and previous period" $ do
      let expected =
            FlowSummary
              { summaryPeriod = (startPeriod, endPeriod),
                summaryFlows = [],
                summaryCommands = []
              }
          startPeriod = LocalTime aDay midnight
          endPeriod = startPeriod & modL month (+ 1)

      getJSON "/api/flows/arnaud/summary?from=1995-10-10&to=1995-11-10&period=Month"
        `shouldRespondWith` ResponseMatcher
          200
          [ "Link"
              <:> encodeUtf8
                ( writeLinkHeader $
                    fromJust $ periodLinks "arnaud" aDay (localDay endPeriod) Month
                )
          ]
          (jsonBodyEquals expected)

    it "PATCH /api/flows/<user>/latest/timestamp updates latest flow's timestamp" $ do
      let flow1 = anOtherFlow
          flow2 = Flow Other "arnaud" (UTCTime aDay 1000) "some/directory"
          trace = Trace "arnaud" (UTCTime aDay 2000) "other/directory" "git" ["bar"] 0 100

      postFlow_ flow1
      postFlow_ flow2
      postTrace_ trace

      let expected = flow1 {_flowTimestamp = UTCTime aDay 400}
          timeshift :: TimeDifference = Minutes (-10)

      patchJSON "/api/flows/arnaud/latest/timestamp" timeshift `shouldRespondJSONBody` expected

    it "GET /api/flows/<user>/latest retrieves latest flow" $ do
      let flow1 = anOtherFlow
          flow2 = Flow Other "arnaud" (UTCTime aDay 1000) "some/directory"

      postFlow_ flow1
      postFlow_ flow2

      getJSON "/api/flows/arnaud/latest" `shouldRespondJSONBody` flow2

    it "GET /api/flows/<user>/2 retrieves flow 2 steps back" $ do
      let flow1 = anOtherFlow
          flow2 = anOtherFlow & later 1000 seconds
          flow3 = anOtherFlow & later 2000 seconds
      postFlow_ flow1
      postFlow_ flow2
      postFlow_ flow3

      getJSON "/api/flows/arnaud/2" `shouldRespondJSONBody` flow1
