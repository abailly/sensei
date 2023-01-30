{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Time.Extra where

import Control.Applicative
import Data.Aeson.Extra
import Data.Aeson.Types
import Data.Csv (FromField(..), ToField(..))
import Data.Maybe
import Data.Monoid
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.ToCSV
import Data.Time.Calendar (Day, addDays, addGregorianMonthsClip, addGregorianYearsClip, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format (ParseTime, defaultTimeLocale, formatTime, iso8601DateFormat, parseTimeM, parseTimeOrError)
import Data.Time.LocalTime hiding (utc)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Prelude as P

defaultTimeZoneOffset :: TimeZoneOffset
defaultTimeZoneOffset = TimeZoneOffset (-240) -- NYC time

defaultTimeZone :: TimeZone
defaultTimeZone = minutesToTimeZone (-240)

-- TODO normalize output to nearest ms
currentDate :: IO Date
currentDate = Date <$> getCurrentTime

currentLocalDate :: IO LocalDate
currentLocalDate = dateToLocalDate defaultTimeZoneOffset <$> currentDate

currentDayInUTC :: IO Date
currentDayInUTC = removeTime <$> currentDate

removeTime :: Date -> Date
removeTime = Date . remove . theDate
  where
    remove utc = utc {utctDayTime = picosecondsToDiffTime 0}

dateToDay :: Date -> Day
dateToDay = utctDay . theDate -- TODO not sure which timezone this is in; assuming local

dayToDate :: Day -> Date
dayToDate day = Date (UTCTime day 0)

overDay :: (Day -> Day) -> Date -> Date
overDay = dimap dateToDay dayToDate

overDate :: (Date -> Date) -> Day -> Day
overDate = dimap dayToDate dateToDay

localDateToDay :: LocalDate -> Day
localDateToDay = localDay . zonedTimeToLocalTime . theLocalDate

-- | Start of Unix 'epoch', the 1/1/1970 as a UTCTime.
--
--  This constant is used to compute timestamps by diffing a date with this start date
--  because @NominalDiffTime@ can easily be saved as a 64 bits value.
unixTime :: UTCTime
unixTime = posixSecondsToUTCTime $ fromIntegral (0 :: Integer)

unixDate :: Date
unixDate = Date unixTime

-- | The number of milliseconds since 'epoch', the start of Unix time
msSinceEpoch :: (Integral a) => UTCTime -> a
msSinceEpoch dt = floor $ diffUTCTime dt unixTime * 1000

-- | Default number of days in a month.
-- For computing in 360/30 calendars
daysInAMonth :: (Num a) => a
daysInAMonth = 30

-- | Simple wrapper over standard time
newtype Date = Date {theDate :: UTCTime}
  deriving (Eq, Show, Read, Generic, Ord)

instance FromField Date where
  parseField bs =
    Date <$> parseTimeM True defaultTimeLocale "%d/%m/%Y" (T.unpack $ T.decodeUtf8 bs)

newtype LocalDate = LocalDate {theLocalDate :: ZonedTime}
  deriving (Show, Read)

instance ToJSON LocalDate where
  toJSON (LocalDate (ZonedTime lt (TimeZone offset _ _))) =
    object
      [ "theDate" .= localTimeToUTC (minutesToTimeZone 0) lt,
        "theTimeZoneOffset" .= offset
      ]

instance FromJSON LocalDate where
  parseJSON v = withObject "LocalDate" (\o -> LocalDate <$> (ZonedTime <$> parseDatePart <*> parseZonePart o)) v
    where
      parseDatePart :: Parser LocalTime
      parseDatePart = utcToLocalTime (minutesToTimeZone 0) . theDate <$> parseJSON v
      parseZonePart :: Object -> Parser TimeZone
      parseZonePart o = ((minutesToTimeZone <$>) <$> o .:? "theTimeZoneOffset") .!= defaultTimeZone

instance Eq LocalDate where
  (LocalDate (ZonedTime lt (TimeZone off _ _))) == (LocalDate (ZonedTime lt' (TimeZone off' _ _))) = lt == lt' && off == off'

dateToLocalDate :: TimeZoneOffset -> Date -> LocalDate
dateToLocalDate (TimeZoneOffset offset) (Date dt) = LocalDate (utcToZonedTime tz dt)
  where
    tz = minutesToTimeZone offset

localDateToDate :: LocalDate -> Date
localDateToDate (LocalDate zt) = Date (zonedTimeToUTC zt)

dateDisplayFormat :: String
dateDisplayFormat = "%-d %b %Y"

instance ToCSV Date where
  toCSV (Date utct) = T.pack $ unsafePerformIO $ formatLocalTime dateDisplayFormat utct

instance ToCSV LocalDate where
  toCSV = T.pack . formatTime defaultTimeLocale dateDisplayFormat . theLocalDate

instance ToJSON Date

instance FromJSON Date

newtype TimeZoneOffset = TimeZoneOffset {fromTimeZoneOffset :: Int} deriving (Show, Read)

-- | Build a new @Date@ given a gregorian date as Year, month, day
--
-- Time offset from midnight is set to 0.
newDay :: Integer -> Int -> Int -> Day
newDay y m d = fromGregorian y m d

newLocalDate :: Integer -> Int -> Int -> LocalDate
newLocalDate y m d = dateToLocalDate defaultTimeZoneOffset (dayToDate (newDay y m d))

lastDayOfSameMonth :: Day -> Day
lastDayOfSameMonth d =
  let (y, m, _) = toGregorian d
   in fromGregorian y m (gregorianMonthLength y m)

isLeapDay :: Day -> Bool
isLeapDay d =
  month d == 2 && dayOfMonth d == 29

-- | Returns the month of given @Date@
month :: Day -> Int
month d =
  let (_, m, _) = toGregorian d
   in m

-- | Returns the day of month of given @Date@
dayOfMonth :: Day -> Int
dayOfMonth d =
  let (_, _, dd) = toGregorian d
   in dd

data DateRange
  = DateRange
      { fromDate :: Date,
        toDate :: Date
      }
  deriving (Generic)

instance ToJSON DateRange

instance FromJSON DateRange

parseISODateString :: (Monad m, MonadFail m, ParseTime t) => String -> m t
parseISODateString = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%QZ")

encodeISODateString :: Date -> String
encodeISODateString = flip dateFormatWith "%H:%M:%S%QZ"

-- | Reads a date in yyyy-MM-dd format
readDate :: String -> Maybe Date
readDate s = Date <$> parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) s

readLocalDate :: String -> Maybe LocalDate
readLocalDate s = LocalDate <$> parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) s

-- like readDate, but throws error; use only in scripts/tests...
unsafeReadDate :: String -> Date
unsafeReadDate s = fromMaybe (error ("unsafeReadDate: cannot parse " <> s)) $ readDate s

readWithFormat :: String -> String -> Date
readWithFormat format dateString = Date $ parseTimeOrError True defaultTimeLocale format dateString

dateFormat :: Date -> String
dateFormat (Date dt) = formatTime defaultTimeLocale (iso8601DateFormat Nothing) dt

dateFormatWith :: Date -> String -> String
dateFormatWith (Date dt) format = formatTime defaultTimeLocale (iso8601DateFormat $ Just format) dt

-- | Compute the number of days between two @Date@s
daysFrom :: Date -> Date -> Int
daysFrom (Date start) (Date end) = truncate $ diffUTCTime end start / (3600 * 24)

-- | Compute the number of months a time span in days represents.
-- Number of months is rounded to nearest integral value.
numberOfMonths :: Int -> Int
numberOfMonths dur =
  let (q, r) = dur `quotRem` daysInAMonth
   in if r >= 15
        then q + 1
        else q

-- | formats a localized time given a format string
formatLocalTime :: String -> UTCTime -> IO String
formatLocalTime formatString time = do
  timeZone <- getCurrentTimeZone
  let format = formatTime defaultTimeLocale formatString
      localTime = utcToLocalTime timeZone time
  return $ format localTime

-- TODO: make this a group
data Period
  = Days Integer
  | Months Integer
  | Years Integer
  | Minutes Integer
  | Hours Integer
  | Seconds Integer
  deriving
    ( Eq,
      Show,
      Read,
      Generic
    )

instance ToJSON Period where
  toJSON (Days days) =
    object
      [ "tag" .= String "days",
        "days" .= days
      ]
  toJSON (Months months) =
    object
      [ "tag" .= String "months",
        "months" .= months
      ]
  toJSON (Years years) =
    object
      [ "tag" .= String "years",
        "years" .= years
      ]
  toJSON (Minutes minutes) =
    object
      [ "tag" .= String "minutes",
        "minutes" .= minutes
      ]
  toJSON (Hours hours) =
    object
      [ "tag" .= String "hours",
        "hours" .= hours
      ]
  toJSON (Seconds seconds) =
    object
      [ "tag" .= String "seconds",
        "seconds" .= seconds
      ]

instance FromJSON Period where
  parseJSON = withObject "Period" $ \o ->
    o .:=> "tag" $ \case
      "days" -> Days <$> o .: "days"
      "months" -> Months <$> o .: "months"
      "years" -> Years <$> o .: "years"
      "minutes" -> Minutes <$> o .: "minutes"
      "hours" -> Hours <$> o .: "hours"
      "seconds" -> Seconds <$> o .: "seconds"
      e -> fail $ "unknown period type while parsing JSON as Period: " <> show e

monthsPosterior :: Integer -> Day -> Day
monthsPosterior = addGregorianMonthsClip

posterior :: Period -> Date -> Date
posterior (Days dy) (Date dt) =
  let dy' = dy `addDays` utctDay dt
      tm = utctDayTime dt
   in Date $ UTCTime dy' tm
posterior (Months m) (Date dt) =
  let m' = m `monthsPosterior` utctDay dt
      tm = utctDayTime dt
   in Date $ UTCTime m' tm
posterior (Years y) (Date dt) =
  let m' = y `addGregorianYearsClip` utctDay dt
      tm = utctDayTime dt
   in Date $ UTCTime m' tm
posterior (Minutes m) (Date dt) = Date $ addUTCTime (fromIntegral $ m * 60) dt
posterior (Hours h) (Date dt) = Date $ addUTCTime (fromIntegral $ h * 60 * 60) dt
posterior (Seconds s) (Date dt) = Date $ addUTCTime (fromIntegral s) dt

infixr 9 `posterior`

anterior :: Period -> Date -> Date
anterior period date = neg period `posterior` date

neg :: Period -> Period
neg (Days dy) = Days (- dy)
neg (Months m) = Months (- m)
neg (Years m) = Years (- m)
neg (Hours h) = Hours (- h)
neg (Minutes m) = Minutes (- m)
neg (Seconds s) = Seconds (- s)

-- getTimeZoneOffset :: (ScottyError e, Monad m) => ActionT e m TimeZoneOffset
-- getTimeZoneOffset = do
--   fromHeader :: Maybe TimeZoneOffset <- ((TimeZoneOffset <$>) . (>>= readMay . L.unpack)) <$> header "X-Timezone-Offset"
--   fromQuery :: Maybe TimeZoneOffset <- (Just <$> param "tzoffset") `rescue` const (return Nothing)
--   return $ fromMaybe defaultTimeZoneOffset (fromHeader <|> fromQuery)

-- getLocalDate :: (ScottyError e, Monad m) => L.Text -> ActionT e m LocalDate
-- getLocalDate paramName = do
--   timezone :: TimeZone <- (minutesToTimeZone . fromTimeZoneOffset) <$> getTimeZoneOffset
--   LocalDate (ZonedTime lt _) <- param paramName
--   return $ LocalDate (ZonedTime lt timezone)

diffDate :: Date -> Date -> Integer
diffDate (Date utc1) (Date utc2) = truncate $ utc1 `diffUTCTime` utc2

instance FromField Day where
  parseField bs =
    parseTimeM True defaultTimeLocale "%m/%d/%Y" (T.unpack $ T.decodeUtf8 bs)

instance ToField Day where
  toField = T.encodeUtf8 . T.pack . formatTime defaultTimeLocale "%m/%d/%Y"
