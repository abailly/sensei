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

import Data.Aeson hiding (Options)
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Numeric.Natural
import Servant

-- | Current version of recorded `Flow` format.
--  This version /must/ be incremented on each change to the structure of `Flow` which
--  impacts the stored representation. Of course, deserialisation
--  functions should be provided in order to migrate data from previous versions.
currentVersion :: Natural
currentVersion = 1

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

instance ToJSON FlowType where
  toJSON (FlowType f) = String f
  toJSON Note = "Note"
  toJSON End = "End"
  toJSON Other = "Other"

instance FromJSON FlowType where
  parseJSON = withText "FlowType" $ \t -> case t of
    "Note" -> pure Note
    "End" -> pure End
    "Other" -> pure Other
    other -> pure $ FlowType other

instance ToHttpApiData FlowType where
  toUrlPiece (FlowType f) = f
  toUrlPiece Note = "Note"
  toUrlPiece End = "End"
  toUrlPiece Other = "Other"

instance FromHttpApiData FlowType where
  parseUrlPiece "End" = pure End
  parseUrlPiece "Note" = pure Note
  parseUrlPiece "Other" = pure Other
  parseUrlPiece other = pure $ FlowType other

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
