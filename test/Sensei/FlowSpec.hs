{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.FlowSpec where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Sensei.API
import Sensei.Generators (generateEvent)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

instance Arbitrary Event where
  arbitrary = do
    d <- choose (40000, 60000)
    s <- choose (0, 3600 * 24)
    let time = UTCTime (toEnum d) (fromInteger s)
    generateEvent time 0

spec :: Spec
spec = describe "Event & Flow" $ do
  it "can serialise/deserialise to/from JSON" $
    lawsCheck (jsonLaws (Proxy @Event))

  it "can deserialize version 4 JSON Event(s)" $ do
    let evs :: Either String [Event] = eitherDecode eventsVersion4JSON
        Right (EventNote note) = head <$> evs

    _noteDir note
      `shouldBe` "w\x0017\x0016\x0007Z!)QN."

eventsVersion4JSON :: LBS.ByteString
eventsVersion4JSON =
  mconcat
    [ "[{\"_flowState\":{\"_flowDir\":\"w\\u0017\\u0016\\u0007Z!)QN.\",\"tag\":\"FlowNote\",\"_flowStart\":\"1995-05-01T10:37:15Z\",\"_flowUser\":\"arnaud\",\"_flowNote\":\"\\u0011\\u0005ES\\u0000'FI\"},\"_flowType\":\"Note\",\"_version\":4},",
      "{\"args\":[\"\\u0017[\",\"\\n\\\\\\u0010i(nB\"],\"exit_code\":-1,\"process\":\">t\\u0005?K\",\"elapsed\":35,\"_version\":4,\"timestamp\":\"2006-02-09T05:37:49Z\",\"directory\":\"2e[\\u0008\"},",
      "{\"_flowState\":{\"_flowDir\":\"\",\"tag\":\"FlowState\",\"_flowStart\":\"2020-11-09T21:38:15Z\",\"_flowUser\":\"arnaud\"},\"_flowType\":\"Rework\",\"_version\":4},",
      "{\"_flowState\":{\"_flowDir\":\"\\u0014Bu\\u0002Ix`\\u001eeR\",\"tag\":\"FlowNote\",\"_flowStart\":\"1970-11-25T21:02:17Z\",\"_flowUser\":\"arnaud\",\"_flowNote\":\"In=boo\"},\"_flowType\":\"Note\",\"_version\":4},",
      "{\"_flowState\":{\"_flowDir\":\"1\",\"tag\":\"FlowState\",\"_flowStart\":\"1968-09-30T01:44:58Z\",\"_flowUser\":\"arnaud\"},\"_flowType\":\"Troubleshooting\",\"_version\":4},",
      "{\"_flowState\":{\"_flowDir\":\"i\\u0006PkO\\r\",\"tag\":\"FlowState\",\"_flowStart\":\"1977-07-20T09:53:04Z\",\"_flowUser\":\"arnaud\"},\"_flowType\":\"Experimenting\",\"_version\":4}]"
    ]
