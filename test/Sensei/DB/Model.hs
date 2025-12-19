{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Sensei.DB.Model where

import Control.Exception.Safe (try)
import Control.Lens (set, (^.))
import Control.Monad (forM_, when)
import Control.Monad.State
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Sequence as Seq
import Data.Text (Text)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Sensei.Generators (generateEvent, generateUserProfile, shiftTime, shrinkEvent, startTime)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec (HasCallStack)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Positive (getPositive),
  Property,
  choose,
  counterexample,
  elements,
  forAllShrinkBlind,
  frequency,
  shrink,
 )
import Test.QuickCheck.Monadic

-- | Relevant commands issued to the underlying DB
data Action a where
  WriteEvent :: Event -> Action ()
  ReadEvents :: Pagination -> Action EventsQueryResult
  ReadFlow :: Reference -> Action (Maybe EventView)
  ReadNotes :: TimeRange -> Action [NoteView]
  ReadGoals :: Action [GoalOp]
  ReadViews :: Action [FlowView]
  ReadCommands :: Action [CommandView]
  NewUser :: UserProfile -> Action ()
  SwitchUser :: Text -> Action ()

instance Show (Action a) where
  show (WriteEvent f) = "WriteEvent " <> show f
  show (ReadEvents page) = "ReadEvents " <> show page
  show (ReadFlow ref) = "ReadFlow " <> show ref
  show (ReadNotes range) = "ReadNotes " <> show range
  show ReadGoals = "ReadGoals"
  show ReadViews = "ReadViews"
  show ReadCommands = "ReadCommands"
  show (NewUser u) = "NewUser " <> show u
  show (SwitchUser u) = "SwitchUser " <> show u

-- | Abstract state against which `Action`s are interpreted and
-- to which underlying storage should conform
data Model = Model
  { currentTimestamp :: UTCTime
  , currentProfile :: UserProfile
  , events :: Seq EventView
  , profiles :: Map.Map Text UserProfile
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
shrinkActions (Actions (as :|> a :|> _)) = Actions . (as :|>) <$> shrinkAction a
shrinkActions _ = []

shrinkAction :: SomeAction -> [SomeAction]
shrinkAction (SomeAction (WriteEvent e)) = SomeAction . WriteEvent <$> shrinkEvent e
shrinkAction (SomeAction (ReadEvents p)) = SomeAction . ReadEvents <$> shrink p
shrinkAction (SomeAction (ReadFlow r)) = SomeAction . ReadFlow <$> shrink r
shrinkAction (SomeAction (ReadNotes _t)) = [] -- SomeAction . ReadNotes <$> shrink t
shrinkAction (SomeAction ReadViews) = []
shrinkAction (SomeAction ReadGoals) = []
shrinkAction (SomeAction ReadCommands) = []
shrinkAction (SomeAction (NewUser p)) = SomeAction . NewUser <$> shrink p
shrinkAction (SomeAction (SwitchUser _)) = []

generateAction :: UTCTime -> Model -> Integer -> Gen (Seq SomeAction)
generateAction baseTime model count =
  go model mempty count
 where
  go :: Model -> Seq SomeAction -> Integer -> Gen (Seq SomeAction)
  go _ acts 0 = pure acts
  go m acts n = do
    act <-
      frequency
        [ (9, SomeAction <$> genEvent m n)
        , (2, SomeAction . ReadFlow <$> arbitrary)
        , (1, arbitrary >>= \p -> pure (SomeAction (ReadEvents p)))
        , (1, choose (0, count - n) >>= \k -> pure $ SomeAction (ReadNotes (TimeRange baseTime (shiftTime baseTime k))))
        , (1, pure $ SomeAction ReadViews)
        , (1, pure $ SomeAction ReadGoals)
        , (1, SomeAction . NewUser <$> generateUserProfile)
        , (3, switchUser m)
        , (1, pure $ SomeAction ReadCommands)
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
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m (Maybe a)
interpret (WriteEvent f) = do
  modify $ \m@Model{events} -> m{events = events |> EventView{index = fromIntegral (Seq.length events + 1), event = f}}
  pure $ Just ()
interpret (ReadFlow Latest) = interpret (ReadFlow (Pos 0))
interpret (ReadFlow (Pos n)) = do
  es <- getEvents
  let fs = Seq.filter (not . isTrace . event) es
  Just <$> findFlowAtPos fs n
interpret (ReadEvents (Page pageNum size)) = do
  es <- getEvents
  let evs = Seq.sortBy eventTimestampDesc es
      totalEvents = fromIntegral $ Seq.length evs
      resultEvents = toList $ Seq.take (fromIntegral size) $ Seq.drop (fromIntegral $ (pageNum - 1) * size) evs
      eventsCount = fromIntegral $ Prelude.length resultEvents
      startIndex = min (fromIntegral $ (pageNum - 1) * size) totalEvents
      endIndex = min (fromIntegral $ pageNum * size) totalEvents
  pure $ Just EventsQueryResult{..}
interpret (ReadEvents NoPagination) = do
  es <- getEvents
  let totalEvents = fromIntegral $ Seq.length es
      resultEvents = toList es
      eventsCount = totalEvents
      startIndex = 1
      endIndex = totalEvents
  pure $ Just EventsQueryResult{..}
interpret (ReadNotes rge) = do
  UserProfile{userName, userTimezone, userProjects} <- gets currentProfile
  fs <- Seq.filter (inRange rge . eventTimestamp . event) <$> getEvents
  pure $ Just $ foldr (notesViewBuilder userName userTimezone userProjects) [] fs
interpret ReadViews = do
  UserProfile{userName, userTimezone, userEndOfDay, userProjects} <- gets currentProfile
  Just . Prelude.reverse . foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay userProjects) [] <$> getEvents
interpret ReadGoals = do
  UserProfile{userName} <- gets currentProfile
  fs <- Seq.filter (liftA2 (&&) isGoal ((== userName) . eventUser) . event) <$> getEvents
  pure $ Just $ mapMaybe (getGoal . event) $ toList fs
interpret ReadCommands = do
  UserProfile{userTimezone, userProjects} <- gets currentProfile
  Just . foldr (commandViewBuilder userTimezone userProjects) [] <$> getEvents
interpret (NewUser u) = do
  pfs <- gets profiles
  case Map.lookup (userName u) pfs of
    Nothing -> modify (\m -> m{currentProfile = u, profiles = Map.insert (userName u) u pfs}) >> pure (Just ())
    Just _ -> pure Nothing
interpret (SwitchUser u) = do
  modify $
    \m ->
      let changeProfile =
            case Map.lookup u (profiles m) of
              Nothing -> currentProfile m
              Just p -> p
       in m{currentProfile = changeProfile}
  pure $ Just ()

getEvents :: Monad m => StateT Model m (Seq EventView)
getEvents = do
  es <- gets events
  UserProfile{userName} <- gets currentProfile
  pure $ Seq.filter ((== userName) . (^. user') . event) es

findFlowAtPos ::
  (Eq a, Num a, Applicative f) =>
  Seq EventView ->
  a ->
  f (Maybe EventView)
findFlowAtPos fs pos =
  case viewr fs of
    rest :> f ->
      if pos == 0
        then pure $ Just f
        else findFlowAtPos rest (pos - 1)
    _ -> pure Nothing

eventTimestampDesc ::
  EventView -> EventView -> Ordering
eventTimestampDesc EventView{event} EventView{event = event'} = desc $ (compare `on` eventTimestamp) event event'
 where
  desc LT = GT
  desc EQ = EQ
  desc GT = LT

runDB :: DB db => Text -> Action a -> db a
runDB _ (WriteEvent f) = writeEvent f
runDB user (ReadEvents p) = readProfileOrDefault user >>= \u -> readEvents u p
runDB user (ReadFlow r) = readProfileOrDefault user >>= \u -> readFlow u r
runDB user (ReadNotes rge) = readProfileOrDefault user >>= \u -> readNotes u rge
runDB user ReadViews = readProfileOrDefault user >>= readViews
runDB user ReadGoals = readProfileOrDefault user >>= readGoals
runDB user ReadCommands = readProfileOrDefault user >>= readCommands
runDB _ (NewUser u) = void $ insertProfile u
runDB _ (SwitchUser _) = pure ()

readProfileOrDefault :: forall db. DB db => Text -> db UserProfile
readProfileOrDefault user = fmap (fromRight defaultProfile) (try @_ @(DBError db) $ readProfile user)

runActions :: DB db => Text -> Actions -> db (Seq String)
runActions user (Actions actions) =
  mapM runAction actions
 where
  runAction (SomeAction act) = show <$> runDB user act

validateActions :: forall db. DB db => Seq SomeAction -> StateT Model db (Seq (Maybe String))
validateActions acts = do
  mapM runAndCheck acts

runAndCheck :: forall db. DB db => SomeAction -> StateT Model db (Maybe String)
runAndCheck (SomeAction act) = do
  UserProfile{userName} <- gets currentProfile
  actual <- either (const Nothing) Just <$> try @_ @(DBError db) (lift $ runDB userName act)
  expected <- interpret act
  if actual == expected
    then pure Nothing
    else do
      pure $
        Just $
          "action = "
            <> show act
            <> ", \nexpected : "
            <> show expected
            <> ", \ngot :  "
            <> show actual

canReadFlowsAndTracesWritten ::
  (DB db, HasCallStack) => FilePath -> (forall x. db x -> IO x) -> Property
canReadFlowsAndTracesWritten dbFile nt =
  forAllShrinkBlind (generateActions start) shrinkActions $ \(Actions actions) -> monadicIO $ do
    let monitorErrors Nothing = pure ()
        monitorErrors (Just s) = monitor (counterexample s)
    res <- run $ do
      hasDB <- doesFileExist dbFile
      when hasDB $ removeFile dbFile
      nt $ initLogStorage >> evalStateT (validateActions actions) start
    forM_ res monitorErrors
    monitor $ counterexample (unlines $ show <$> toList actions)
    assert $ all isNothing res
 where
  start = Model startTime defaultProfile mempty (Map.singleton (userName defaultProfile) defaultProfile)
