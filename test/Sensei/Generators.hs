{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Generators where

import Control.Lens ((.~))
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), addUTCTime)
import Data.Time.LocalTime
import Preface.Codec (Base64, Encoded (..), Hex, toBase64, toHex)
import Sensei.API
import Sensei.ColorSpec ()
import Sensei.DB
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    choose,
    elements,
    frequency,
    getASCIIString,
    getPositive,
    getPrintableString,
    listOf,
    oneof,
    resize,
    vectorOf,
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

genUserId :: Gen (Encoded Hex)
genUserId = toHex . BS.pack <$> vectorOf 16 arbitrary

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
    <*> genUserId

instance Arbitrary UserProfile where
  arbitrary = generateUserProfile

  shrink u@(UserProfile _ _ _ _ fs cs _ _) =
    ((\f -> u {userFlowTypes = f}) <$> shrink fs)
      <> ((\c -> u {userCommands = c}) <$> shrink cs)

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
  pure $ EventTrace $ Trace "arnaud" st dir pr args ex el

generateArgs :: Gen [Text]
generateArgs = listOf $ pack . getASCIIString <$> arbitrary

generateProcess :: Gen Text
generateProcess = pack . getASCIIString <$> arbitrary

instance Arbitrary Pagination where
  arbitrary =
    frequency
      [ (10, Page <$> genNatural <*> genNatural),
        (1, pure NoPagination)
      ]

  shrink (Page n s) = Page <$> shrink n <*> shrink s
  shrink NoPagination = []

instance Arbitrary Reference where
  arbitrary =
    frequency
      [ (3, pure Latest),
        (1, Pos <$> genNatural)
      ]

  shrink Latest = []
  shrink (Pos p) = Pos <$> shrink p

generateEvent :: UTCTime -> Integer -> Gen Event
generateEvent baseTime off =
  oneof [generateTrace baseTime off, generateFlow baseTime off]

shrinkEvent :: Event -> [Event]
shrinkEvent (EventTrace t@(Trace _ _ _ _ args _ _)) =
  [EventTrace $ t & traceArgs .~ x | x <- shrink args]
shrinkEvent _ = []
