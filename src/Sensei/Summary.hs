{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sensei.Summary where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Sensei.Flow

-- | A summary of flows and other events for a given period of time
data FlowSummary = FlowSummary
  { summaryPeriod :: (Day, Day),
    summaryFlows :: [(FlowType, NominalDiffTime)],
    summaryCommands :: [(Text, NominalDiffTime)]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | An interface for things that can be /summarized/
class HasSummary event key where
  -- | group a sequence of @event@s by a @key@, accumulating elapsed
  --  time
  summarize :: [event] -> [(key, NominalDiffTime)]
