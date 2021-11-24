{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A representation of all types of `Event` related to users' activity.
module Sensei.Event
(Event(..),
     currentVersion,
    eventUser,
    setUser,
    user',
    eventTimestamp,
    isTrace,
    filterNotes,
    parseEventFromv4,
    parseNoteFromv4,
    parseFlowFromv4,
)
  where

import Control.Applicative
import Control.Lens (Lens', set)
import Data.Aeson hiding (Options)
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Data.Time
import GHC.Generics
import Numeric.Natural
import Sensei.FlowType
import Sensei.Flow


-- | Current version of data storage format.
--
-- This version /must/ be incremented on each change to the structure of `Event` and
-- other stored data structures which
-- impacts their serialized representation. Of course, deserialisation
-- functions should be provided in order to migrate data from previous versions.
currentVersion :: Natural
currentVersion = 8

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

isTrace :: Event -> Bool
isTrace EventTrace {} = True
isTrace _ = False

-- | Project a stream of 'Event' into a stream of 'NoteFlow'
filterNotes :: [Event] -> [NoteFlow]
filterNotes = foldr addNote []
  where
    addNote (EventNote note) notes = note : notes
    addNote _ notes = notes
