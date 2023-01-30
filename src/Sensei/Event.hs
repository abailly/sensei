{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A representation of all types of `Event` related to users' activity.
module Sensei.Event (
    Event (..),
    eventUser,
    setUser,
    user',
    eventTimestamp,
    isTrace,
    isGoal,
    getGoal,
    filterNotes,
    parseEventFromv4,
    parseNoteFromv4,
    parseFlowFromv4,
) where

import Control.Applicative
import Control.Lens (Lens', set)
import Data.Aeson hiding (Options)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as K
import Data.Aeson.Types
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Numeric.Natural
import Sensei.Flow
import Sensei.Goal
import Sensei.Version (currentVersion)

-- | Common type grouping all kind of core events that are stored in the DB
data Event
    = EventFlow Flow
    | EventTrace Trace
    | EventNote NoteFlow
    | EventGoal GoalOp
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
                        "Goal" -> EventGoal <$> parseJSON (Object obj)
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
                pure $ EventFlow fl{_flowType = ty}
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
                K.insert (fromText "tag") "Flow" $
                    K.insert (fromText "version") (toJSON currentVersion) obj
    toJSON (EventTrace t) =
        let Object obj = toJSON t
         in Object $
                K.insert (fromText "tag") "Trace" $
                    K.insert (fromText "version") (toJSON currentVersion) obj
    toJSON (EventNote n) =
        let Object obj = toJSON n
         in Object $
                K.insert (fromText "tag") "Note" $
                    K.insert (fromText "version") (toJSON currentVersion) obj
    toJSON (EventGoal g) =
        let Object obj = toJSON g
         in Object $
                K.insert (fromText "tag") "Goal" $
                    K.insert (fromText "version") (toJSON currentVersion) obj

eventTimestamp ::
    Event -> UTCTime
eventTimestamp (EventFlow f) = _flowTimestamp f
eventTimestamp (EventTrace t) = _traceTimestamp t
eventTimestamp (EventNote n) = _noteTimestamp n
eventTimestamp (EventGoal g) = _goalTimestamp g

eventUser ::
    Event -> Text
eventUser (EventFlow f) = _flowUser f
eventUser (EventTrace t) = _traceUser t
eventUser (EventNote n) = _noteUser n
eventUser (EventGoal g) = _goalUser g

setUser :: Text -> Event -> Event
setUser u (EventFlow f) = EventFlow $ f{_flowUser = u}
setUser u (EventTrace t) = EventTrace $ t{_traceUser = u}
setUser u (EventNote n) = EventNote $ n{_noteUser = u}
setUser u (EventGoal g) = EventGoal $ g{_goalUser = u}

user' :: Lens' Event Text
user' fu (EventFlow f@Flow{_flowUser}) = (\u -> EventFlow (set flowUser u f)) <$> fu _flowUser
user' fu (EventTrace t@Trace{_traceUser}) = (\u -> EventTrace (set traceUser u t)) <$> fu _traceUser
user' fu (EventNote n@NoteFlow{_noteUser}) = (\u -> EventNote (set noteUser u n)) <$> fu _noteUser
user' fu (EventGoal n@GoalOp{_goalUser}) = (\u -> EventGoal (set goalUser u n)) <$> fu _goalUser

isTrace :: Event -> Bool
isTrace EventTrace{} = True
isTrace _ = False

isGoal :: Event -> Bool
isGoal EventGoal{} = True
isGoal _ = False

getGoal :: Event -> Maybe GoalOp
getGoal (EventGoal g) = Just g
getGoal _ = Nothing

-- | Project a stream of 'Event' into a stream of 'NoteFlow'
filterNotes :: [Event] -> [NoteFlow]
filterNotes = foldr addNote []
  where
    addNote (EventNote note) notes = note : notes
    addNote _ notes = notes
