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
import Data.Text (Text)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Sensei.Generators (genNatural, generateEvent, generateUserProfile, shiftTime, startTime)
import Test.Hspec (HasCallStack)
import Test.QuickCheck
  ( Arbitrary (arbitrary, shrink),
    Gen,
    Positive (getPositive),
    Property,
    choose,
    counterexample,
    frequency,
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
  NewUser :: UserProfile -> Action ()

instance Show (Action a) where
  show (WriteEvent f) = "WriteEvent " <> show f
  show (ReadEvents page) = "ReadEvents " <> show page
  show (ReadFlow ref) = "ReadFlow " <> show ref
  show (ReadNotes range) = "ReadNotes " <> show range
  show ReadViews = "ReadViews"
  show ReadCommands = "ReadCommands"
  show (NewUser u) = "NewUser " <> show u

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

newtype Actions = Actions {actions :: Seq SomeAction}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary =
    Actions <$> (arbitrary >>= sequence . fromList . map (generateAction startTime) . enumFromTo 1 . getPositive)

  shrink (Actions (as :|> _)) = [Actions as]
  shrink _ = []

generateAction :: UTCTime -> Integer -> Gen SomeAction
generateAction baseTime k =
  frequency
    [ (9, SomeAction . WriteEvent <$> generateEvent baseTime k),
      (2, pure $ SomeAction (ReadFlow Latest)),
      (1, (,) <$> genNatural <*> genNatural >>= \(n, s) -> pure (SomeAction (ReadEvents (Page n s)))),
      (1, choose (0, k) >>= \n -> pure $ SomeAction (ReadNotes (TimeRange baseTime (shiftTime baseTime n)))),
      (1, pure $ SomeAction ReadViews),
      (1, SomeAction . NewUser <$> generateUserProfile),
      (1, pure $ SomeAction ReadCommands)
    ]

-- | Interpret a sequence of actions against a `Model`,
-- yielding a new, updated, `Model`
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m a
interpret (WriteEvent f) = modify $ \m@Model {events} -> m {events = events |> f}
interpret (ReadFlow Latest) = do
  fs <- gets (Seq.filter (not . isTrace) . events)
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
interpret (ReadEvents NoPagination) = do
  es <- gets events
  let totalEvents = fromIntegral $ Seq.length es
      resultEvents = toList es
      eventsCount = totalEvents
      startIndex = 1
      endIndex = totalEvents
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
interpret (NewUser u) =
  modify $ \m -> m {currentProfile = u}

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
runDB (NewUser u) = void $ newUser u

readProfileOrDefault :: DB db => db UserProfile
readProfileOrDefault = fmap (either (const defaultProfile) id) (readProfile "user")

runActions :: (DB db) => Actions -> db (Seq String)
runActions (Actions actions) =
  sequence $ runAction <$> actions
  where
    runAction (SomeAction act) = show <$> runDB act

validateActions :: forall db. DB db => Seq SomeAction -> StateT Model db (Seq (Maybe String))
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
