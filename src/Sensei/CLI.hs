{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI where

import Data.Aeson hiding (Options, Success)
import Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Format.ISO8601
import Options.Applicative
import Sensei.API
import Sensei.CLI.Terminal
import Sensei.Client
import Sensei.Version
import System.Exit
import System.IO
import System.IO.Unsafe

data Options
  = QueryOptions {queryDay :: Maybe Day, summarize :: Bool, groups :: [Group]}
  | RecordOptions {recordType :: FlowType}
  | NotesOptions {notesDay :: Day, format :: NoteFormat}
  | UserOptions {userAction :: UserAction}
  deriving (Show, Eq)

data UserAction
  = GetProfile
  | SetProfile {profileFile :: FilePath}
  | GetVersions
  | ShiftTimestamp TimeDifference
  | GetFlow Reference
  deriving (Show, Eq)

runOptionsParser ::
  Maybe [FlowType] -> [String] -> Either Text Options
runOptionsParser flows arguments =
  case execParserPure defaultPrefs (optionsParserInfo flows) arguments of
    Success opts -> Right opts
    Failure f -> Left (Text.pack . show $ execFailure f "")
    _ -> Left "Unexpected completion invoked"

optionsParserInfo :: Maybe [FlowType] -> ParserInfo Options
optionsParserInfo flows =
  info
    (optionsParser flows <**> helper)
    ( progDesc $
        unlines
          [ "ep(oché) - Record start time of some flow type for current user",
            "version: " <> showVersion senseiVersion <> ", storage: " <> show currentVersion
          ]
    )

optionsParser :: Maybe [FlowType] -> Parser Options
optionsParser flows =
  QueryOptions <$> optional dayParser <*> summarizeParser <*> many groupParser
    <|> RecordOptions <$> flowTypeParser flows
    <|> NotesOptions <$> dayParser <* notesParser <*> formatParser
    <|> UserOptions <$> (profileParser *> userActionParser)

{-# INLINE today #-}
today :: Day
today = unsafePerformIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

formatParser :: Parser NoteFormat
formatParser =
  option
    (eitherReader parseNoteFormat)
    ( long "format"
        <> short 'f'
        <> metavar "STRING"
        <> help "notes formatting, can be 'plain' (default) or 'table'"
    )

dayParser :: Parser Day
dayParser =
  option
    (maybeReader iso8601ParseM)
    ( long "date"
        <> short 'd'
        <> metavar "DATE"
        <> value today
        <> help "date to filter on, in ISO8601 format (YYYY-mm-dd)"
    )

summarizeParser :: Parser Bool
summarizeParser =
  flag
    False
    True
    ( long "summary"
        <> short 's'
        <> help "summarize by flow type"
    )

groupParser :: Parser Group
groupParser =
  option
    auto
    ( long "group"
        <> short 'G'
        <> metavar "GROUP"
        <> help "groups for retrieving daily views, one of Week, Month, Quarter or Year"
    )

notesParser :: Parser ()
notesParser =
  flag
    ()
    ()
    ( long "notes"
        <> short 'N'
        <> help "Display only notes for a given day"
    )

flowTypeParser ::
  Maybe [FlowType] -> Parser FlowType
flowTypeParser (fromMaybe defaultFlowTypes -> flows) =
  foldr mkFlag baseFlag flows
  where
    keyLetter (FlowType "") = '.'
    keyLetter (FlowType (Text.head . Text.toLower -> l)) = l
    keyLetter Other = 'o'
    keyLetter Note = 'n'
    keyLetter End = 'E'

    mkFlag ftype parser =
      flag' ftype (short (keyLetter ftype) <> help (show ftype <> " period")) <|> parser

    baseFlag =
      flag' End (short 'E' <> help "End previous period")
        <|> flag' Note (short 'n' <> help "Taking some note")
        <|> flag' Other (short 'o' <> help "Other period")

profileParser :: Parser ()
profileParser =
  flag
    ()
    ()
    ( long "user-profile"
        <> short 'U'
        <> help "get or set the user profile for current user"
    )

userActionParser :: Parser UserAction
userActionParser =
  ( SetProfile
      <$> strOption
        ( long "config"
            <> short 'c'
            <> metavar "FILE"
            <> help "JSON-formatted user profile to use"
        )
      <|> pure GetProfile
  )
    <|> flag'
      GetVersions
      ( long "versions"
          <> short 'v'
          <> help "retrieve the current versions of client and server"
      )
    <|> ( ShiftTimestamp
            <$> option
              (eitherReader parse)
              ( long "shift-time"
                  <> short 'S'
                  <> help "shift the latest flow by the given time difference"
              )
        )
    <|> ( GetFlow
            <$> option
              (eitherReader parseRef)
              ( long "query"
                  <> short 'Q'
                  <> value Latest
                  <> help "Query some flow or group of FlowViews from underlying storage (default: latest)"
              )
        )

parseSenseiOptions ::
  UserProfile -> IO Options
parseSenseiOptions userProfile = execParser (optionsParserInfo $ userDefinedFlows userProfile)

display :: ToJSON a => a -> IO ()
display = LBS.putStr . encodePretty

ep :: Options -> Text -> UTCTime -> Text -> IO ()
ep (QueryOptions Nothing False grps) usrName _ _ =
  send (queryFlowC usrName grps) >>= display
ep (QueryOptions Nothing True _) usrName _ _ =
  send (queryFlowSummaryC usrName) >>= display
ep (QueryOptions (Just day) False _) usrName _ _ =
  send (queryFlowDayC usrName day) >>= display
ep (QueryOptions (Just day) True _) usrName _ _ =
  send (queryFlowDaySummaryC usrName day) >>= display
ep (NotesOptions day noteFormat) usrName _ _ =
  send (notesDayC usrName day) >>= mapM_ println . fmap encodeUtf8 . formatNotes noteFormat
ep (RecordOptions ftype) curUser startDate curDir =
  case ftype of
    Note -> do
      txt <- captureNote
      send $ flowC curUser Note (FlowNote curUser startDate curDir txt)
    other ->
      send $ flowC curUser other (FlowState curUser startDate curDir)
ep (UserOptions GetProfile) usrName _ _ =
  send (getUserProfileC usrName) >>= display
ep (UserOptions (SetProfile file)) usrName _ _ = do
  profile <- eitherDecode <$> LBS.readFile file
  case profile of
    Left err -> hPutStrLn stderr ("failed to decode user profile from " <> file <> ": " <> err) >> exitWith (ExitFailure 1)
    Right prof -> void $ send (setUserProfileC usrName prof)
ep (UserOptions GetVersions) _ _ _ = do
  vs <- send getVersionsC
  display vs {clientVersion = senseiVersion, clientStorageVersion = currentVersion}
ep (UserOptions (ShiftTimestamp diff)) curUser _ _ = do
  f <- send (updateFlowC curUser diff)
  display f
ep (UserOptions (GetFlow q)) curUser _ _ = do
  f <- send (getFlowC curUser q)
  display f

println :: BS.ByteString -> IO ()
println bs =
  BS.putStr bs >> BS.putStr "\n"

formatNotes :: NoteFormat -> [NoteView] -> [Text]
formatNotes Plain = concatMap timestamped
formatNotes MarkdownTable = (tblHeaders <>) . fmap tblRow
formatNotes Section = concatMap sectionized

sectionized :: NoteView -> [Text]
sectionized (NoteView ts note) =
  ["#### " <> formatTimestamp ts, "", note]

tblHeaders :: [Text]
tblHeaders = ["Time | Note", "--- | ---"]

tblRow :: NoteView -> Text
tblRow (NoteView ts note) = formatTimestamp ts <> " | " <> replaceEOLs note

replaceEOLs :: Text -> Text
replaceEOLs = Text.replace "\n" "<br/>"

formatTimestamp :: LocalTime -> Text
formatTimestamp = Text.pack . formatTime defaultTimeLocale "%H:%M"

timestamped :: NoteView -> [Text]
timestamped (NoteView st note) = formatTimestamp st : Text.lines note
