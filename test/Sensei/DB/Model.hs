{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sensei.DB.Model where

import Control.Lens (set, (^.))
import Control.Monad.State
import Data.Foldable (toList)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Sequence as Seq
import Data.Text (Text)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Sensei.Generators (genNatural, generateEvent, generateUserProfile, shiftTime, startTime)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec (HasCallStack)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Positive (getPositive),
    Property,
    choose,
    counterexample,
    elements,
    forAllShrink,
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
  SwitchUser :: Text -> Action ()

instance Show (Action a) where
  show (WriteEvent f) = "WriteEvent " <> show f
  show (ReadEvents page) = "ReadEvents " <> show page
  show (ReadFlow ref) = "ReadFlow " <> show ref
  show (ReadNotes range) = "ReadNotes " <> show range
  show ReadViews = "ReadViews"
  show ReadCommands = "ReadCommands"
  show (NewUser u) = "NewUser " <> show u
  show (SwitchUser u) = "SwitchUser " <> show u

-- | Abstract state against which `Action`s are interpreted and
-- to which underlying storage should conform
data Model = Model
  { currentTimestamp :: UTCTime,
    currentProfile :: UserProfile,
    events :: Seq Event,
    profiles :: Map.Map Text UserProfile
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

generateActions :: Model -> Gen Actions
generateActions model =
  Actions <$> (arbitrary >>= generateAction startTime model . getPositive)

shrinkActions :: Actions -> [Actions]
shrinkActions (Actions (as :|> _)) = [Actions as]
shrinkActions _ = []

generateAction :: UTCTime -> Model -> Integer -> Gen (Seq SomeAction)
generateAction baseTime model count =
  go model mempty count
  where
    go :: Model -> Seq SomeAction -> Integer -> Gen (Seq SomeAction)
    go _ acts 0 = pure acts
    go m acts n = do
      act <-
        frequency
          [ (9, SomeAction <$> genEvent m n),
            (2, pure $ SomeAction (ReadFlow Latest)),
            (1, (,) <$> genNatural <*> genNatural >>= \(p, s) -> pure (SomeAction (ReadEvents (Page p s)))),
            (1, choose (0, (count - n)) >>= \k -> pure $ SomeAction (ReadNotes (TimeRange baseTime (shiftTime baseTime k)))),
            (1, pure $ SomeAction ReadViews),
            (1, SomeAction . NewUser <$> generateUserProfile),
            (3, switchUser m),
            (1, pure $ SomeAction ReadCommands)
          ]
      m' <- step m act
      go m' (acts :|> act) (n - 1)

    switchUser m = SomeAction . SwitchUser <$> elements (Map.keys $ profiles m)

    step m (SomeAction a) =
      execStateT (interpret a) m

    genEvent m n = do
      e <- generateEvent baseTime (count - n)
      pure $ WriteEvent $ set user' (userName $ currentProfile m) e

-- | Interpret a sequence of actions against a `Model`,
-- yielding a new, updated, `Model`
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m a
interpret (WriteEvent f) = modify $ \m@Model {events} -> m {events = events |> f}
interpret (ReadFlow Latest) = do
  es <- getEvents
  let fs = Seq.filter (not . isTrace) es
  case viewr fs of
    _ :> f@EventFlow {} -> pure $ Just f
    _ :> n@EventNote {} -> pure $ Just n
    _ -> pure Nothing
interpret (ReadFlow _) = error "not implemented"
interpret (ReadEvents (Page pageNum size)) = do
  es <- getEvents
  let evs = Seq.sortBy eventTimestampDesc es
      totalEvents = fromIntegral $ Seq.length evs
      resultEvents = toList $ Seq.take (fromIntegral size) $ Seq.drop (fromIntegral $ (pageNum - 1) * size) evs
      eventsCount = fromIntegral $ Prelude.length resultEvents
      startIndex = min (fromIntegral $ (pageNum - 1) * size) totalEvents
      endIndex = min (fromIntegral $ pageNum * size) totalEvents
  pure EventsQueryResult {..}
interpret (ReadEvents NoPagination) = do
  es <- getEvents
  let totalEvents = fromIntegral $ Seq.length es
      resultEvents = toList es
      eventsCount = totalEvents
      startIndex = 1
      endIndex = totalEvents
  pure EventsQueryResult {..}
interpret (ReadNotes rge) = do
  UserProfile {userName, userTimezone} <- gets currentProfile
  fs <- Seq.filter (inRange rge . eventTimestamp) <$> getEvents
  pure $ foldr (notesViewBuilder userName userTimezone) [] fs
interpret ReadViews = do
  UserProfile {userName, userTimezone, userEndOfDay} <- gets currentProfile
  fs <- getEvents
  pure $ Prelude.reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay) [] fs
interpret ReadCommands = do
  UserProfile {userTimezone} <- gets currentProfile
  ts <- getEvents
  pure $ foldr (commandViewBuilder userTimezone) [] ts
interpret (NewUser u) =
  modify $ \m -> m {currentProfile = u}
interpret (SwitchUser u) =
  modify $
    \m ->
      let changeProfile =
            case Map.lookup u (profiles m) of
              Nothing -> currentProfile m
              Just p -> p
       in m {currentProfile = changeProfile}

getEvents :: Monad m => StateT Model m (Seq Event)
getEvents = do
  es <- gets events
  UserProfile {userName} <- gets currentProfile
  pure $ Seq.filter ((== userName) . (^. user')) es

eventTimestampDesc ::
  Event -> Event -> Ordering
eventTimestampDesc e e' = desc $ (compare `on` eventTimestamp) e e'
  where
    desc LT = GT
    desc EQ = EQ
    desc GT = LT

runDB :: (DB db) => Text -> Action a -> db a
runDB _ (WriteEvent f) = writeEvent f
runDB user (ReadEvents p) = readProfileOrDefault user >>= \u -> readEvents u p
runDB user (ReadFlow r) = readProfileOrDefault user >>= \u -> readFlow u r
runDB user (ReadNotes rge) = readProfileOrDefault user >>= \u -> readNotes u rge
runDB user ReadViews = readProfileOrDefault user >>= readViews
runDB user ReadCommands = readProfileOrDefault user >>= readCommands
runDB _ (NewUser u) = void $ writeProfile u
runDB _ (SwitchUser _) = pure ()

readProfileOrDefault :: DB db => Text -> db UserProfile
readProfileOrDefault user = fmap (either (const defaultProfile) id) (readProfile user)

runActions :: (DB db) => Text -> Actions -> db (Seq String)
runActions user (Actions actions) =
  sequence $ runAction <$> actions
  where
    runAction (SomeAction act) = show <$> runDB user act

validateActions :: forall db. DB db => Seq SomeAction -> StateT Model db (Seq (Maybe String))
validateActions acts = do
  sequence $ runAndCheck <$> acts

runAndCheck :: DB db => SomeAction -> StateT Model db (Maybe String)
runAndCheck (SomeAction act) = do
  UserProfile {userName} <- gets currentProfile
  actual <- lift $ runDB userName act
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
  (DB db, HasCallStack) => FilePath -> (forall x. db x -> IO x) -> Property
canReadFlowsAndTracesWritten dbFile nt =
  forAllShrink (generateActions start) shrinkActions $ \(Actions actions) -> monadicIO $ do
    let monitorErrors Nothing = pure ()
        monitorErrors (Just s) = monitor (counterexample s)
    res <- run $ do
      hasDB <- doesFileExist dbFile
      when hasDB $ removeFile dbFile
      nt $ initLogStorage >> evalStateT (validateActions actions) start
    forM_ res monitorErrors
    assert $ all isNothing res
  where
    start = Model startTime defaultProfile mempty (Map.singleton (userName defaultProfile) defaultProfile)
