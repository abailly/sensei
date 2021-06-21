{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sensei.DB.Model where

import Control.Monad.State
import Data.Foldable (toList)
import Data.Function (on)
import Data.Maybe (isNothing)
import Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Test.Hspec (HasCallStack)
import Test.QuickCheck
  ( ASCIIString (getASCIIString),
    Arbitrary (arbitrary),
    Gen,
    Positive (getPositive),
    Property,
    choose,
    counterexample,
    elements,
    frequency,
    listOf,
    oneof,
  )
import Test.QuickCheck.Monadic

-- | Relevant commands issued to the underlying DB
data Action a where
  WriteEvent :: Event -> Action ()
  ReadEvents :: Pagination -> Action EventsQueryResult
  ReadFlow :: Reference -> Action (Maybe Event)
  ReadNotes :: TimeRange -> Action [(LocalTime, Text)]
  ReadViews :: Action [FlowView]
  ReadCommands :: Action [CommandView]

instance Show (Action a) where
  show (WriteEvent f) = "WriteEvent " <> show f
  show (ReadEvents page) = "ReadEvents " <> show page
  show (ReadFlow ref) = "ReadFlow " <> show ref
  show (ReadNotes range) = "ReadNotes " <> show range
  show ReadViews = "ReadViews"
  show ReadCommands = "ReadCommands"

-- | Abstract state against which `Action`s are interpreted and
-- to which underlying storage should conform
data Model = Model
  { currentTimestamp :: UTCTime,
    currentProfile :: UserProfile,
    events :: Seq Event
  }
  deriving (Eq, Show)

data SomeAction where
  SomeAction :: forall a. (Eq a, Show a) => Action a -> SomeAction

instance Show SomeAction where
  show (SomeAction act) = show act

instance Eq SomeAction where
  _ == _ = False

data Actions = Actions {actions :: [SomeAction]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary =
    Actions <$> (arbitrary >>= sequence . map (generateAction startTime) . enumFromTo 1 . getPositive)

genNatural :: Gen Natural
genNatural = fromInteger . getPositive <$> arbitrary

startTime :: UTCTime
startTime = UTCTime (toEnum 50000) 10000

generateAction :: UTCTime -> Integer -> Gen SomeAction
generateAction baseTime k =
  frequency
    [ (9, SomeAction . WriteEvent <$> generateEvent baseTime k),
      (2, pure $ SomeAction (ReadFlow Latest)),
      (1, (,) <$> genNatural <*> genNatural >>= \(n, s) -> pure (SomeAction (ReadEvents (Page n s)))),
      (1, choose (0, k) >>= \n -> pure $ SomeAction (ReadNotes (TimeRange baseTime (shiftTime baseTime n)))),
      (1, pure $ SomeAction ReadViews),
      (1, pure $ SomeAction ReadCommands)
    ]

instance Arbitrary FlowType where
  arbitrary =
    frequency
      [ (4, elements defaultFlowTypes),
        (3, pure Note),
        (1, pure End),
        (2, pure Other)
      ]

generateFlow :: UTCTime -> Integer -> Gen Event
generateFlow baseTime k = do
  typ <- arbitrary
  case typ of
    Note -> EventNote <$> generateNote baseTime k
    _ -> EventFlow <$> generateState typ baseTime k

shiftTime :: UTCTime -> Integer -> UTCTime
shiftTime baseTime k = addUTCTime (fromInteger $ k * 1000) baseTime

generateNote :: UTCTime -> Integer -> Gen NoteFlow
generateNote baseTime k = do
  let st = shiftTime baseTime k
  dir <- generateDir
  note <- generateNoteText
  pure $ NoteFlow "arnaud" st dir note

generateState :: FlowType -> UTCTime -> Integer -> Gen Flow
generateState ftype baseTime k = do
  let st = shiftTime baseTime k
  dir <- generateDir
  pure $ Flow ftype "arnaud" st dir -- TODO: remove user from Flow definition

generateDir :: Gen Text
generateDir = pack . getASCIIString <$> arbitrary

generateNoteText :: Gen Text
generateNoteText = pack . getASCIIString <$> arbitrary

generateTrace :: UTCTime -> Integer -> Gen Event
generateTrace baseTime k = do
  let st = shiftTime baseTime k
  dir <- generateDir
  pr <- generateProcess
  args <- generateArgs
  ex <- arbitrary
  el <- fromInteger <$> choose (0, 100)
  pure $ EventTrace $ Trace "arnaud" st (unpack dir) pr args ex el

generateArgs :: Gen [Text]
generateArgs = listOf $ pack . getASCIIString <$> arbitrary

generateProcess :: Gen Text
generateProcess = pack . getASCIIString <$> arbitrary

generateEvent :: UTCTime -> Integer -> Gen Event
generateEvent baseTime offset =
  oneof [generateTrace baseTime offset, generateFlow baseTime offset]

-- | Interpret a sequence of actions against a `Model`,
-- yielding a new, updated, `Model`
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m a
interpret (WriteEvent f) = modify $ \m@Model {events} -> m {events = events |> f}
interpret (ReadFlow Latest) = do
  fs <- Seq.filter (not . isTrace) <$> gets events
  case viewr fs of
    _ :> f@EventFlow {} -> pure $ Just f
    _ :> n@EventNote {} -> pure $ Just n
    _ -> pure Nothing
interpret (ReadFlow _) = error "not implemented"
interpret (ReadEvents (Page pageNum size)) = do
  es <- gets events
  let evs = Seq.sortBy eventTimestampDesc es
      totalEvents = fromIntegral $ Seq.length evs
      resultEvents = toList $ Seq.take (fromIntegral size) $ Seq.drop (fromIntegral $ (pageNum - 1) * size) evs
      eventsCount = fromIntegral $ Prelude.length resultEvents
      startIndex = min (fromIntegral $ (pageNum - 1) * size) totalEvents
      endIndex = min (fromIntegral $ pageNum * size) totalEvents
  pure EventsQueryResult {..}
interpret (ReadNotes rge) = do
  UserProfile {userName, userTimezone} <- gets currentProfile
  fs <- Seq.filter (inRange rge . eventTimestamp) <$> gets events
  pure $ foldr (notesViewBuilder userName userTimezone) [] fs
interpret ReadViews = do
  UserProfile {userName, userTimezone, userEndOfDay} <- gets currentProfile
  fs <- gets events
  pure $ Prelude.reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay) [] fs
interpret ReadCommands = do
  UserProfile {userTimezone} <- gets currentProfile
  ts <- gets events
  pure $ foldr (commandViewBuilder userTimezone) [] ts

eventTimestampDesc ::
  Event -> Event -> Ordering
eventTimestampDesc e e' = desc $ (compare `on` eventTimestamp) e e'
  where
    desc LT = GT
    desc EQ = EQ
    desc GT = LT

runDB :: (DB db) => Action a -> db a
runDB (WriteEvent f) = writeEvent f
runDB (ReadEvents p) = readProfileOrDefault >>= \u -> readEvents u p
runDB (ReadFlow r) = readProfileOrDefault >>= \u -> readFlow u r
runDB (ReadNotes rge) = readProfileOrDefault >>= \u -> readNotes u rge
runDB ReadViews = readProfileOrDefault >>= readViews
runDB ReadCommands = readProfileOrDefault >>= readCommands

readProfileOrDefault :: DB db => db UserProfile
readProfileOrDefault = fmap (either (const defaultProfile) id) readProfile

runActions :: (DB db) => Actions -> db [String]
runActions (Actions actions) =
  sequence $ runAction <$> actions
  where
    runAction (SomeAction act) = show <$> runDB act

validateActions :: forall db. DB db => [SomeAction] -> StateT Model db [Maybe String]
validateActions acts = do
  sequence $ runAndCheck <$> acts

runAndCheck :: DB db => SomeAction -> StateT Model db (Maybe String)
runAndCheck (SomeAction act) = do
  actual <- lift $ runDB act
  expected <- interpret act
  if actual == expected
    then pure Nothing
    else do
      m <- get
      pure $
        Just $
          "with state = " <> show m
            <> ", \naction = "
            <> show act
            <> ", \nexpected : "
            <> show expected
            <> ", \ngot :  "
            <> show actual

canReadFlowsAndTracesWritten ::
  (DB db, HasCallStack) => (forall x. db x -> IO x) -> Actions -> Property
canReadFlowsAndTracesWritten nt (Actions actions) = monadicIO $ do
  let start = Model startTime defaultProfile mempty
      monitorErrors Nothing = pure ()
      monitorErrors (Just s) = monitor (counterexample s)
  res <- run $ nt $ initLogStorage >> evalStateT (validateActions actions) start
  forM_ res monitorErrors
  assert $ all isNothing res
