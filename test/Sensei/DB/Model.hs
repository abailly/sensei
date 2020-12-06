{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sensei.DB.Model where

import Control.Monad.State
import Data.Sequence
import Data.Text (Text, unpack, pack)
import Data.Time
import Sensei.API hiding ((|>))
import Sensei.DB
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

generateFlow :: UTCTime -> Int -> Gen Flow
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

generateNote :: UTCTime -> Int -> Gen FlowState
generateNote baseTime k = do
  usr <- generateUser
  st <- pure $ addUTCTime (toEnum $ k * 1000) baseTime
  dir <- generateDir
  note <- generateNoteText
  pure $ FlowNote usr st dir note

generateState :: UTCTime -> Int -> Gen FlowState
generateState baseTime k = do
  usr <- generateUser
  st <- pure $ addUTCTime (toEnum $ k * 1000) baseTime
  dir <- generateDir
  pure $ FlowState usr st dir

generateUser :: Gen Text
generateUser = elements ["arnaud", "alice"]

generateDir :: Gen Text
generateDir = pack . getPrintableString <$> arbitrary

generateNoteText :: Gen Text
generateNoteText = pack . getPrintableString <$> arbitrary

generateAction :: UTCTime -> Int -> Gen SomeAction
generateAction baseTime k =
  frequency
    [ (9, SomeAction . WriteFlow <$> generateFlow baseTime k),
      (1, pure $ SomeAction ReadNotes)
    ]

instance Arbitrary Actions where
  arbitrary =
    Actions <$> (listOf arbitrary >>= sequence . map (generateAction startTime))

-- | Interpret a sequence of actions against a `Model`,
-- yielding a new, updated, `Model`
interpret :: (Monad m, Eq a, Show a) => Action a -> StateT Model m a
interpret (WriteFlow f) = modify $ \m@Model {flows} -> m {flows = flows |> f}
--interpret (WriteTrace t) = modify $ \ m@Model{traces} -> m { traces = traces |> t }
interpret ReadNotes = do
  UserProfile {userName, userTimezone} <- gets currentProfile
  fs <- gets flows
  pure $ foldr (notesViewBuilder userName userTimezone) [] fs
interpret _ = undefined

runDB :: (DB db) => Action a -> db a
runDB (WriteFlow f) = writeFlow f
runDB (WriteTrace t) = writeTrace t
runDB ReadNotes = readProfileOrError >>= readNotes
runDB ReadViews = readProfileOrError >>= readViews
runDB ReadCommands = readProfileOrError >>= readCommands

readProfileOrError :: DB db => db UserProfile
readProfileOrError = fmap (either (error . unpack) id) readProfile

validateActions :: forall db. DB db => [SomeAction] -> StateT Model db [Bool]
validateActions acts = do
  sequence $ runAndCheck <$> acts

runAndCheck :: DB db => SomeAction -> StateT Model db Bool
runAndCheck (SomeAction act) = do
  actual <- lift $ runDB act
  expected <- interpret act
  pure $ actual == expected

canReadFlowsAndTracesWritten ::
  (DB db) => (forall x. db x -> IO x) -> Actions -> Property
canReadFlowsAndTracesWritten nt (Actions actions) = monadicIO $ do
  let start = Model startTime defaultProfile mempty mempty
  res <- run $ nt $ evalStateT (validateActions actions) start
  assert $ and res
