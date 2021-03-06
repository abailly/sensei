{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Sensei.Summary where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Time
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

  -- |group a sequence of @event@s by a @key@, accumulating elapsed
  -- time
  summarize :: [event] -> [(key, NominalDiffTime)]
