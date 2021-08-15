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
    Event (..),
    Trace (..),
    Reference (..),
    parseNoteFormat,
    defaultFlowTypes,
    currentVersion,
    eventUser,
    setUser,
    user',
    eventTimestamp,
    isTrace,
    parseEventFromv4,
    parseNoteFromv4,
    parseFlowType,
    parseFlowFromv4,
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

import Control.Applicative
import Control.Lens (Lens', set)
import Control.Lens.TH (makeLenses)
import Data.Aeson hiding (Options)
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (first))
import qualified Data.HashMap.Strict as H
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Numeric.Natural
import Sensei.FlowType
import Servant

-- | Current version of data storage format.
--
-- This version /must/ be incremented on each change to the structure of `Event` and
-- other stored data structures which
-- impacts their serialized representation. Of course, deserialisation
-- functions should be provided in order to migrate data from previous versions.
currentVersion :: Natural
currentVersion = 8

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

eventTimestamp ::
  Event -> UTCTime
eventTimestamp (EventFlow f) = _flowTimestamp f
eventTimestamp (EventTrace t) = _traceTimestamp t
eventTimestamp (EventNote n) = _noteTimestamp n

eventUser ::
  Event -> Text
eventUser (EventFlow f) = _flowUser f
eventUser (EventTrace t) = _traceUser t
eventUser (EventNote n) = _noteUser n

setUser :: Text -> Event -> Event
setUser u (EventFlow f) = EventFlow $ f {_flowUser = u}
setUser u (EventTrace t) = EventTrace $ t {_traceUser = u}
setUser u (EventNote n) = EventNote $ n {_noteUser = u}

user' :: Lens' Event Text
user' fu (EventFlow f@Flow {_flowUser}) = (\u -> EventFlow (set flowUser u f)) <$> fu _flowUser
user' fu (EventTrace t@Trace {_traceUser}) = (\u -> EventTrace (set traceUser u t)) <$> fu _traceUser
user' fu (EventNote n@NoteFlow {_noteUser}) = (\u -> EventNote (set noteUser u n)) <$> fu _noteUser

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

-- | Common type grouping all kind of core events that are stored in the DB
data Event
  = EventFlow Flow
  | EventTrace Trace
  | EventNote NoteFlow
  deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON =
    withObject "Event" $ \obj -> do
      v :: Natural <- obj .: "version" <|> obj .: "_version"
      case v of
        4 -> parseEventFromv4 obj
        _ -> do
          t <- obj .: "tag"
          case t of
            "Note" -> EventNote <$> parseJSON (Object obj)
            "Trace" -> EventTrace <$> parseJSON (Object obj)
            "Flow" -> EventFlow <$> parseJSON (Object obj)
            o -> fail ("cannot parse Event with tag " <> o)

parseEventFromv4 :: Object -> Parser Event
parseEventFromv4 obj =
  parseTrace <|> parseFlow
  where
    parseFlow = do
      ty <- obj .: "_flowType"
      st <- obj .: "_flowState"
      case ty of
        Note -> EventNote <$> parseNoteFromv4 st
        _ -> do
          fl <- parseFlowFromv4 st
          pure $ EventFlow fl {_flowType = ty}
    parseTrace = do
      ts <- obj .: "timestamp"
      el <- obj .: "elapsed"
      ar <- obj .: "args"
      pr <- obj .: "process"
      dr <- obj .: "directory"
      ex <- obj .: "exit_code"
      pure $ EventTrace $ Trace "" ts dr pr ar ex el

parseNoteFromv4 :: Value -> Parser NoteFlow
parseNoteFromv4 =
  withObject "NoteFlow" $ \state -> do
    NoteFlow
      <$> state .: "_flowUser"
      <*> state .: "_flowStart"
      <*> state .: "_flowDir"
      <*> state .: "_flowNote"

parseFlowFromv4 :: Value -> Parser Flow
parseFlowFromv4 =
  withObject "Flow" $ \state -> do
    Flow <$> pure Other
      <*> state .: "_flowUser"
      <*> state .: "_flowStart"
      <*> state .: "_flowDir"

instance ToJSON Event where
  toJSON (EventFlow f) =
    let Object obj = toJSON f
     in Object $
          H.insert "tag" "Flow" $
            H.insert "version" (toJSON currentVersion) obj
  toJSON (EventTrace t) =
    let Object obj = toJSON t
     in Object $
          H.insert "tag" "Trace" $
            H.insert "version" (toJSON currentVersion) obj
  toJSON (EventNote n) =
    let Object obj = toJSON n
     in Object $
          H.insert "tag" "Note" $
            H.insert "version" (toJSON currentVersion) obj

isTrace :: Event -> Bool
isTrace EventTrace {} = True
isTrace _ = False
