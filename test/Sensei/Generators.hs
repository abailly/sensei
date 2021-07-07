{-# LANGUAGE OverloadedStrings #-}

module Sensei.Generators where

import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime (..), addUTCTime)
import Data.Time.LocalTime
import Preface.Codec (Base64, Encoded (..), toBase64)
import Sensei.API
import Sensei.ColorSpec ()
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    choose,
    elements,
    frequency,
    getPrintableString,
    getASCIIString,
    getPositive,
    listOf,
    oneof,
    resize,
  )

-- * Orphan Instances

generateUser :: Gen Text
generateUser = resize 20 (pack . getPrintableString <$> arbitrary)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = TimeOfDay <$> choose (0, 11) <*> choose (0, 59) <*> (fromInteger <$> choose (0, 59))

genTimeZone :: Gen TimeZone
genTimeZone = hoursToTimeZone <$> choose (- 12, 12)

genPassword :: Gen (Encoded Base64, Encoded Base64)
genPassword = (,) <$> genBase64 <*> genBase64

genBase64 :: Gen (Encoded Base64)
genBase64 = toBase64 <$> arbitrary

generateUserProfile :: Gen UserProfile
generateUserProfile =
  UserProfile
    <$> generateUser
    <*> genTimeZone
    <*> genTimeOfDay
    <*> genTimeOfDay
    <*> arbitrary
    <*> arbitrary
    <*> genPassword

instance Arbitrary UserProfile where
  arbitrary = generateUserProfile

instance Arbitrary FlowType where
  arbitrary =
    frequency
      [ (4, elements defaultFlowTypes),
        (3, pure Note),
        (1, pure End),
        (2, pure Other)
      ]

genNatural :: Gen Natural
genNatural = fromInteger . getPositive <$> arbitrary

startTime :: UTCTime
startTime = UTCTime (toEnum 50000) 10000

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
generateEvent baseTime off =
  oneof [generateTrace baseTime off, generateFlow baseTime off]
