{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Flows represent the various recorded events that are relevant to capture
-- from a user's daily activity.
--
-- Currently records the following types of flows:
--
-- * `Trace`: common command-line programs execution recording
-- * `Flow`: start time of a specific (expected) type of activity
-- * `FlowNote`: notes
module Sensei.Flow
  ( FlowType (..),
    Flow (..),
    NoteFlow (..),
    NoteFormat (..),
    Trace (..),
    Reference (..),
    parseNoteFormat,
    defaultFlowTypes,
    parseFlowType,
    parseRef,
    flowDir,
    flowTimestamp,
    flowType,
    flowUser,
    traceArgs,
    traceDirectory,
    traceExitCode,
    traceElapsed,
    traceProcess,
    traceTimestamp,
    traceUser,
    noteUser,
    noteTimestamp,
    noteDir,
    noteContent,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson hiding (Options)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Numeric.Natural
import Sensei.FlowType
import Servant

data Flow = Flow
  { _flowType :: FlowType,
    _flowUser :: Text,
    _flowTimestamp :: UTCTime,
    _flowDir :: Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''Flow

instance FromJSON Flow where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Flow where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data Trace = Trace
  { _traceUser :: Text,
    _traceTimestamp :: UTCTime,
    _traceDirectory :: Text,
    _traceProcess :: Text,
    _traceArgs :: [Text],
    _traceExitCode :: Int,
    _traceElapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic)

makeLenses ''Trace

instance FromJSON Trace where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Trace where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data NoteFlow = NoteFlow
  { _noteUser :: Text,
    _noteTimestamp :: UTCTime,
    _noteDir :: Text,
    _noteContent :: Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''NoteFlow

instance FromJSON NoteFlow where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON NoteFlow where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}


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

-- | Reference to an item in the log
data Reference
  = -- | Refers to the latest entry in the log
    Latest
  | -- | Refers to the item located `offset` positions from the `Latest` entry
    Pos {offset :: Natural}
  deriving (Eq, Show)

instance ToJSON Reference where
  toJSON = String . toUrlPiece

instance FromJSON Reference where
  parseJSON = withText "Reference" $ either (fail . unpack) pure . parseUrlPiece

instance ToHttpApiData Reference where
  toUrlPiece Latest = "latest"
  toUrlPiece (Pos n) = Text.pack $ show n

instance FromHttpApiData Reference where
  parseUrlPiece "latest" = pure Latest
  parseUrlPiece txt = Pos <$> parseUrlPiece txt

parseRef :: String -> Either String Reference
parseRef = first Text.unpack . parseUrlPiece . Text.pack

