{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Flows represent the various recorded events that are relevant to capture
-- from a user's daily activity.
--
-- Currently records the following types of flows:
--
-- * `Trace`: common command-line programs execution recording
-- * `Flow`: either notes or start time of a specific (expected) type of activity
module Sensei.Flow where

import Control.Applicative
import Data.Aeson hiding (Options)
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ToText
import Data.Time
import GHC.Generics
import Numeric.Natural
import Servant

-- | Current version of data storage format.
-- This version /must/ be incremented on each change to the structure of `Flow` and
-- other stored data structures which
-- impacts their serialized representation. Of course, deserialisation
-- functions should be provided in order to migrate data from previous versions.
currentVersion :: Natural
currentVersion = 4

-- | Common type grouping all kind of events that are stored in the DB
-- TODO: This type is in an early stage and only used currently when migrating
-- database, refactor when exposing the full log to the user. In particular
-- the `Flow` and `Trace` types should be unified.
data Event
  = F Flow
  | T Trace
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON (F flow) = toJSON flow
  toJSON (T trace) = toJSON trace

instance FromJSON Event where
  parseJSON v = T <$> parseJSON v <|> F <$> parseJSON v

eventTimestamp ::
  Event -> UTCTime
eventTimestamp (F Flow{_flowState}) = _flowStart _flowState
eventTimestamp (T Trace{timestamp}) = timestamp

-- | Execution "trace" of a program
data Trace = Trace
  { timestamp :: UTCTime,
    directory :: FilePath,
    process :: Text,
    args :: [Text],
    exit_code :: Int,
    elapsed :: NominalDiffTime,
    _version :: Natural
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState,
    _version :: Natural
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FlowType
  = FlowType Text
  | Note
  | End
  | Other
  deriving (Eq, Show, Ord)

instance ToJSONKey FlowType where
  toJSONKey = toJSONKeyText toUrlPiece

instance FromJSONKey FlowType where
  fromJSONKey = FromJSONKeyTextParser parseFlowType

instance ToJSON FlowType where
  toJSON (FlowType f) = String f
  toJSON Note = "Note"
  toJSON End = "End"
  toJSON Other = "Other"

instance ToText FlowType where
  toText (FlowType f) = f
  toText Note = "Note"
  toText End = "End"
  toText Other = "Other"

parseFlowType :: Applicative f => Text -> f FlowType
parseFlowType t =
  case t of
    "Note" -> pure Note
    "End" -> pure End
    "Other" -> pure Other
    other -> pure $ FlowType other

instance FromJSON FlowType where
  parseJSON = withText "FlowType" $ parseFlowType

instance ToHttpApiData FlowType where
  toUrlPiece (FlowType f) = f
  toUrlPiece Note = "Note"
  toUrlPiece End = "End"
  toUrlPiece Other = "Other"

instance FromHttpApiData FlowType where
  parseUrlPiece = parseFlowType

-- | Default flow types when user does not define her own list
-- These are the flow types available for recording, on top of the
-- standard ones which are `Note`, `End` and `Other`
defaultFlowTypes :: [FlowType]
defaultFlowTypes =
  FlowType
    <$> [ "Experimenting",
          "Troubleshooting",
          "Flowing",
          "Rework",
          "Meeting",
          "Learning"
        ]

data FlowState
  = FlowState
      { _flowUser :: Text,
        _flowStart :: UTCTime,
        _flowDir :: Text
      }
  | FlowNote
      { _flowUser :: Text,
        _flowStart :: UTCTime,
        _flowDir :: Text,
        _flowNote :: Text
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Supported rendering formats for notes
data NoteFormat
  = -- | Timestamp of note is on its own line, followed by note as it is typed
    Plain
  | -- | Notes are formatted as a <https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#tables Markdown table>
    --  with the first column containing the timestamp and second containing the text.
    --  EOLs are replaced with @<br/>@ so that underlying formatter can cope with newlines embedded in table cells. this
    --  might or might not work depending on flavor of markdown
    MarkdownTable
  | -- | Notes are formatted with the time as a level 4 section header and
    --  the notes within the section
    Section
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

parseNoteFormat :: String -> Either String NoteFormat
parseNoteFormat = first Text.unpack . parseUrlPiece . Text.pack

instance ToHttpApiData NoteFormat where
  toUrlPiece Plain = "plain"
  toUrlPiece MarkdownTable = "table"
  toUrlPiece Section = "section"

instance FromHttpApiData NoteFormat where
  parseUrlPiece "plain" = pure Plain
  parseUrlPiece "table" = pure MarkdownTable
  parseUrlPiece "section" = pure Section
  parseUrlPiece txt = Left $ "Unknown format: " <> txt

-- |Reference to an item in the log
data Reference =
  Latest
  -- ^Refers to the latest entry in the log
  | Pos { offset :: Natural }
  -- ^Refers to the item located `offset` positions from the `Latest` entry
  deriving (Eq, Show)

instance ToHttpApiData Reference where
  toUrlPiece Latest = "latest"
  toUrlPiece (Pos n) = Text.pack $ show n

instance FromHttpApiData Reference where
  parseUrlPiece "latest" = pure Latest
  parseUrlPiece txt = Pos <$> parseUrlPiece txt

parseRef :: String -> Either String Reference
parseRef = first Text.unpack . parseUrlPiece . Text.pack
