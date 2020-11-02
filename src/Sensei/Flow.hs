{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sensei.Flow where

import Data.Aeson hiding (Options)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Servant


data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Rework | Note | Other | Meeting | End
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
  parseUrlPiece "Rework" = pure Rework
  parseUrlPiece "Note" = pure Note
  parseUrlPiece "End" = pure End
  parseUrlPiece "Meeting" = pure Meeting
  parseUrlPiece _txt = pure Other

data FlowState
  = FlowState
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String
      }
  | FlowNote
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String,
        _flowNote :: Text.Text
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
