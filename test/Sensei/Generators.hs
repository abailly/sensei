{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sensei.Generators where

import Control.Lens ((.~))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as Base32
import Data.Function ((&))
import qualified Data.List as List
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.LocalTime
import Network.URI.Extra (URI (..), URIAuth (..))
import Preface.Codec (Base64, Encoded (..), Hex, toBase64, toHex)
import Sensei.API
import Sensei.Backend (Backend (..))
import Sensei.Bsky
  ( AtURI (..),
    BackgroundImage (..),
    Blob (..),
    BlobRef (..),
    Block (..),
    BlockVariant (..),
    BskyAuth (..),
    BskyBackend (BskyBackend),
    BskyLogin (..),
    ByteSlice (..),
    DID (..),
    Document (..),
    Facet (..),
    Feature (..),
    LinearDocument (..),
    Page (..),
    RichText (..),
    StrongRef (..),
    TID (..),
    TextAlignment (..),
    Theme (..),
    base32SortableAlphabet,
  )
import Sensei.Bsky.CID (CID, computeCID)
import qualified Sensei.Bsky as Bsky
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
    suchThat,
    vectorOf,
  )
import Prelude hiding (exp)

-- * Orphan Instances

generateUser :: Gen Text
generateUser = resize 20 (pack . getPrintableString <$> arbitrary)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = TimeOfDay <$> choose (0, 11) <*> choose (0, 59) <*> (fromInteger <$> choose (0, 59))

genTimeZone :: Gen TZLabel
genTimeZone =
  toEnum
    <$> choose
      ( fromEnum (minBound @TZLabel),
        fromEnum (maxBound @TZLabel)
      )

genPassword :: Gen (Encoded Base64, Encoded Base64)
genPassword = (,) <$> genBase64 <*> genBase64

genBase64 :: Gen (Encoded Base64)
genBase64 = toBase64 . BS.pack <$> arbitrary

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
    <*> arbitrary
    <*> genPassword
    <*> genUserId
    <*> arbitrary

instance Arbitrary UserProfile where
  arbitrary = generateUserProfile

  shrink u@(UserProfile _ _ _ _ fs cs ps _ _ bks) =
    ((\f -> u {userFlowTypes = f}) <$> shrink fs)
      <> ((\c -> u {userCommands = c}) <$> shrink cs)
      <> ((\p -> u {userProjects = p}) <$> shrink ps)
      <> ((\b -> u {backends = b}) <$> shrink bks)

instance Arbitrary FlowType where
  arbitrary =
    frequency
      [ (4, elements defaultFlowTypes),
        (3, pure Note),
        (1, pure End),
        (2, pure Other)
      ]

instance Arbitrary ProjectName where
  arbitrary = ProjectName <$> resize 20 (pack . getPrintableString <$> arbitrary)

-- we don't really want to build arbitrary complex regexes so let's just
-- do simple stuff, and keep the regex smalls otherwise we'll slow the tests
-- as hell
instance Arbitrary Regex where
  arbitrary = Regex . pack . concat <$> reFragments
    where
      reFragments :: Gen [String]
      reFragments = vectorOf 3 reFragment

      reFragment = oneof [pure ".*", listOf (elements ['a' .. 'z'])]

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

instance Arbitrary Natural where
  arbitrary = fromInteger . getPositive <$> arbitrary
  shrink nat = fromInteger <$> shrink (fromIntegral nat)

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

generateArticle :: UTCTime -> Integer -> Gen Article
generateArticle baseTime k = do
  let st = shiftTime baseTime k
  dir <- generateDir
  -- TODO: Generate article content (markdown)
  title <- pack <$> arbitrary
  content <- pack <$> arbitrary
  date <- oneof [pure Nothing, Just <$> generateDate]
  let articleContent = "# " <> title <> "\n\n" <> content
  -- Generate all three types of Article constructors
  frequency
    [ (5, pure $ PublishArticle "arnaud" st dir articleContent date),
      ( 3,
        do
          -- For UpdateArticle, generate a TID-like string
          tidStr <- pack . take 13 <$> arbitrary
          pure $ UpdateArticle "arnaud" st dir tidStr articleContent date
      ),
      ( 2,
        do
          -- For DeleteArticle, generate a TID-like string
          tidStr <- pack . take 13 <$> arbitrary
          pure $ DeleteArticle "arnaud" st dir tidStr
      )
    ]

generateDate :: Gen UTCTime
generateDate =
  (UTCTime . toEnum <$> arbitrary) <*> (fromInteger . getPositive <$> arbitrary)

generateEvent :: UTCTime -> Integer -> Gen Event
generateEvent baseTime off =
  oneof [generateTrace baseTime off, generateFlow baseTime off, EventArticle <$> generateArticle baseTime off]

shrinkEvent :: Event -> [Event]
shrinkEvent (EventTrace t@(Trace _ _ _ _ args _ _)) =
  [EventTrace $ t & traceArgs .~ (Text.pack <$> x) | x <- shrink (Text.unpack <$> args)]
shrinkEvent _ = []

instance Arbitrary Backend where
  arbitrary = Backend <$> oneof [arbitrary @BskyBackend]

instance Arbitrary BskyBackend where
  arbitrary =
    BskyBackend <$> genLogin <*> genURI <*> arbitrary <*> genAtURI

newtype SimpleString = SimpleString {getSimpleString :: String}
  deriving (Eq, Show)

instance Arbitrary SimpleString where
  arbitrary =
    SimpleString
      <$> listOf
        ( oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')]
        )

genLogin :: Gen BskyLogin
genLogin = BskyLogin <$> genIdentifier <*> genPwd
  where
    genIdentifier = pack . getSimpleString <$> arbitrary
    genPwd = pack . getSimpleString <$> arbitrary

genURI :: Gen URI
genURI = do
  uriScheme <- genURIScheme
  uriAuthority <- genURIAuthority
  uriPath <- genURIPath
  pure URI {uriQuery = "", uriFragment = "", ..}
  where
    genURIScheme = elements ["http:", "https:"]

    genURIAuthority = do
      uriRegName <- genURIRegName
      uriPort <- genURIPort
      pure $ Just URIAuth {uriUserInfo = "", ..}

    genURIPath = do
      numSegments <- choose (1, 10)
      ("/" <>) . List.intercalate "/" <$> vectorOf numSegments (getSimpleString <$> arbitrary)

    genURIPort =
      maybe "" show <$> frequency [(9, pure Nothing), (1, Just <$> choose (1 :: Int, 65535))]

genURIRegName :: Gen String
genURIRegName = do
  numSegments <- choose (1, 5)
  List.intercalate "." <$> vectorOf numSegments ((getSimpleString <$> arbitrary) `suchThat` \s -> length s < 30)

instance Arbitrary BskyAuth where
  arbitrary = do
    let scope = "com.atproto.access"
    sub <- ("did:plc:" <>) . Base32.encodeBase32 . BS.pack <$> vectorOf 10 arbitrary
    iat <- choose (1732500000, 1732600000)
    let exp = iat + 6200
    aud <- ("did:web:" <>) . Text.pack <$> genURIRegName
    pure $ BskyAuth {..}

-- * Leaflet Arbitrary Instances

-- | Generate a simple Text value for testing
genSimpleText :: Gen Text
genSimpleText = resize 100 (pack . getPrintableString <$> arbitrary)

-- | Generate a longer Text value for document content
genLongText :: Gen Text
genLongText = resize 500 (pack . getPrintableString <$> arbitrary)

instance Arbitrary ByteSlice where
  arbitrary = do
    start <- choose (0, 100)
    end <- choose (start + 1, start + 50)
    pure $ ByteSlice start end

instance Arbitrary Feature where
  arbitrary =
    oneof
      [ Link <$> genSimpleText,
        DidMention <$> (("did:plc:" <>) <$> genSimpleText),
        AtMention <$> (("at://" <>) <$> genSimpleText),
        pure Code,
        pure Highlight,
        pure Underline,
        pure Strikethrough,
        Id <$> frequency [(1, pure Nothing), (1, Just <$> genSimpleText)],
        pure Bold,
        pure Italic
      ]

instance Arbitrary Facet where
  arbitrary = Facet <$> arbitrary <*> resize 3 (listOf arbitrary)

instance Arbitrary RichText where
  arbitrary = RichText <$> genLongText <*> arbitrary

instance Arbitrary TextAlignment where
  arbitrary = elements [TextAlignLeft, TextAlignCenter, TextAlignRight, TextAlignJustify]

instance Arbitrary BlockVariant where
  -- Only support TextBlock for now
  arbitrary = TextBlock <$> arbitrary

instance Arbitrary Block where
  arbitrary = Block <$> arbitrary <*> arbitrary

instance Arbitrary LinearDocument where
  arbitrary = LinearDocument <$> arbitrary <*> resize 10 (listOf arbitrary)

instance Arbitrary Page where
  arbitrary =
    frequency
      [ (9, Linear <$> arbitrary),
        (1, pure Canvas)
      ]

instance Arbitrary StrongRef where
  arbitrary = StrongRef <$> genSimpleText <*> genSimpleText

instance Arbitrary Bsky.Color where
  arbitrary = Bsky.Color <$> choose (0, 255) <*> choose (0, 255) <*> choose (0, 255) <*> arbitrary

instance Arbitrary CID where
  arbitrary = do
    -- Generate random bytes and compute CID
    bytes <- BS.pack <$> vectorOf 32 arbitrary
    pure $ computeCID bytes

instance Arbitrary BlobRef where
  arbitrary = BlobRef <$> arbitrary

instance Arbitrary Blob where
  arbitrary = Blob <$> elements ["image/png", "image/jpeg", "image/gif"] <*> choose (1, 1000000) <*> arbitrary

instance Arbitrary BackgroundImage where
  arbitrary = BackgroundImage <$> arbitrary

instance Arbitrary Theme where
  arbitrary =
    Theme
      <$> arbitrary -- backgroundColor
      <*> arbitrary -- backgroundImage
      <*> arbitrary -- primary
      <*> arbitrary -- pageBackground
      <*> arbitrary -- showPageBackground
      <*> arbitrary -- accentBackground
      <*> arbitrary -- accentText

instance Arbitrary Document where
  arbitrary =
    Document
      <$> genSimpleText -- title
      <*> frequency [(1, pure Nothing), (1, Just <$> genSimpleText)] -- description
      <*> (("did:plc:" <>) <$> genSimpleText) -- author
      <*> resize 5 (listOf arbitrary) -- pages
      <*> frequency [(1, pure Nothing), (1, Just <$> listOf genSimpleText)] -- tags
      <*> frequency [(1, pure Nothing), (1, Just <$> genSimpleText)] -- publishedAt
      <*> arbitrary -- postRef
      <*> frequency [(1, pure Nothing), (1, Just <$> genSimpleText)] -- publication
      <*> arbitrary -- theme

-- | Arbitrary instance for QuickCheck testing
instance Arbitrary TID where
  arbitrary = do
    chars <- vectorOf 13 (elements $ Text.unpack base32SortableAlphabet)
    return $ TID (Text.pack chars)

instance Arbitrary DID where
  arbitrary = do
    -- Generate 24 random base32 characters (matching the format)
    chars <- vectorOf 24 (elements "abcdefghijklmnopqrstuvwxyz234567")
    pure $ DID $ "did:plc:" <> Text.pack chars

genAtURI :: Gen AtURI
genAtURI = do
  DID did <- arbitrary
  lexicon <- pack <$> genURIRegName
  TID tid <- arbitrary
  pure $ AtURI $ "at://" <> did <> "/" <> lexicon <> "/" <> tid
