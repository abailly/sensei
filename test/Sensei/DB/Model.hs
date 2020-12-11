{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sensei.DB.Model where

import Control.Monad.State
import Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Test.QuickCheck
    (listOf, choose,  Arbitrary(arbitrary),
      Gen,
      frequency,
      Property,
      counterexample,
      elements,
      Positive(getPositive),
      ASCIIString(getASCIIString) )
import Test.QuickCheck.Monadic
import Data.Maybe (isNothing)

-- | Relevant commands issued to the underlying DB
data Action a where
  WriteFlow :: Flow -> Action ()
  WriteTrace :: Trace -> Action ()
  ReadNotes :: Action [(LocalTime, Text)]
  ReadViews :: Action [FlowView]
  ReadCommands :: Action [CommandView]

instance Show (Action a) where
  show (WriteFlow f) = "WriteFlow " <> show f
  show (WriteTrace t) = "WriteTrace " <> show t
  show ReadNotes = "ReadNotes"
  show ReadViews = "ReadViews"
  show ReadCommands = "ReadCommands"

-- | Abstract state against which `Action`s are interpreted and
-- to which underlying storage should conform
data Model = Model
  { currentTimestamp :: UTCTime,
    currentProfile :: UserProfile,
    flows :: Seq Flow,
    traces :: Seq Trace
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

startTime :: UTCTime
startTime = UTCTime (toEnum 50000) 10000

generateFlow :: UTCTime -> Integer -> Gen Flow
generateFlow baseTime k = do
  typ <-
    frequency
      [ (4, elements defaultFlowTypes),
        (3, pure Note),
        (1, pure End),
        (2, pure Other)
      ]
  st <- case typ of
    Note -> generateNote baseTime k
    _ -> generateState baseTime k
  pure $ Flow typ st currentVersion

generateNote :: UTCTime -> Integer -> Gen FlowState
generateNote baseTime k = do
  st <- pure $ addUTCTime (fromInteger $ k * 1000) baseTime
  dir <- generateDir
  note <- generateNoteText
  pure $ FlowNote "arnaud" st dir note

generateState :: UTCTime -> Integer -> Gen FlowState
generateState baseTime k = do
  st <- pure $ addUTCTime (fromInteger $ k * 1000) baseTime
  dir <- generateDir
  pure $ FlowState "arnaud" st dir  -- TODO: remove user from Flow definition

generateDir :: Gen Text
generateDir = pack . getASCIIString <$> arbitrary

generateNoteText :: Gen Text
generateNoteText = pack . getASCIIString <$> arbitrary

generateAction :: UTCTime -> Integer -> Gen SomeAction
generateAction baseTime k =
  frequency
    [ (9, SomeAction . WriteFlow <$> generateFlow baseTime k),
      (7, SomeAction . WriteTrace <$> generateTrace baseTime k),
      (1, pure $ SomeAction ReadNotes),
      (1, pure $ SomeAction ReadViews),
      (1, pure $ SomeAction ReadCommands)
    ]

generateTrace :: UTCTime -> Integer -> Gen Trace
generateTrace  baseTime k = do
  st <- pure $ addUTCTime (fromInteger $ k * 1000) baseTime
  dir <- generateDir
  pr <- generateProcess
  args <- generateArgs
  ex <- arbitrary
  el <- fromInteger <$> choose (0, 100)
  pure $ Trace st (unpack dir) pr args ex el currentVersion

generateArgs :: Gen [Text]
generateArgs = listOf $ pack . getASCIIString <$> arbitrary

generateProcess :: Gen Text
generateProcess = pack . getASCIIString <$> arbitrary

instance Arbitrary Actions where
  arbitrary =
    Actions <$> (arbitrary >>= sequence . map (generateAction startTime) . enumFromTo 1 . getPositive)

-- | Interpret a sequence of actions against a `Model`,
-- yielding a new, updated, `Model`
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m a
interpret (WriteFlow f) = modify $ \m@Model {flows} -> m {flows = flows |> f}
interpret (WriteTrace t) = modify $ \ m@Model{traces} -> m { traces = traces |> t }
interpret ReadNotes = do
  UserProfile {userName, userTimezone} <- gets currentProfile
  fs <- gets flows
  pure $ foldr (notesViewBuilder userName userTimezone) [] fs
interpret ReadViews = do
  UserProfile {userName, userTimezone, userEndOfDay} <- gets currentProfile
  fs <- gets flows
  pure $ Prelude.reverse $ foldl (flip $ flowViewBuilder userName userTimezone userEndOfDay) [] fs
interpret ReadCommands = do
  UserProfile {userTimezone} <- gets currentProfile
  ts <- gets traces
  pure $ foldr (commandViewBuilder userTimezone) [] ts

runDB :: (DB db) => Action a -> db a
runDB (WriteFlow f) = writeFlow f
runDB (WriteTrace t) = writeTrace t
runDB ReadNotes = readProfileOrDefault >>= readNotes
runDB ReadViews = readProfileOrDefault >>= readViews
runDB ReadCommands = readProfileOrDefault >>= readCommands

readProfileOrDefault :: DB db => db UserProfile
readProfileOrDefault = fmap (either (const defaultProfile) id) readProfile

runActions :: (DB db) => Actions -> db [String]
runActions (Actions actions) =
  sequence $ runAction  <$> actions
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
    pure $ Just $ "with state = " <> show m <>
      ", expected : " <> show expected <>
      ", got :  " <> show actual

canReadFlowsAndTracesWritten ::
  (DB db) => (forall x. db x -> IO x) -> Actions -> Property
canReadFlowsAndTracesWritten nt (Actions actions) = monadicIO $ do
  let start = Model startTime defaultProfile mempty mempty
      monitorErrors Nothing = pure ()
      monitorErrors (Just s) = monitor (counterexample s)
  res <- run $ nt $ initLogStorage >> evalStateT (validateActions actions) start
  forM_ res monitorErrors
  assert $ all isNothing res
