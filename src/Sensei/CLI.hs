{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI where

import qualified Control.Exception.Safe as Exc
import Data.Aeson hiding (Options, Success)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Format.ISO8601
import Options.Applicative
import Sensei.API
import Sensei.Client
import System.Console.ANSI
import System.IO
import System.IO.Unsafe

data Options
  = QueryOptions {queryDay :: Maybe Day, summarize :: Bool, groups :: [Group]}
  | RecordOptions {recordType :: FlowType}
  | NotesOptions {notesDay :: Day, format :: NoteFormat}
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
    (progDesc "Epoché - Record start time of some flow type for current user")

optionsParser :: Maybe [FlowType] -> Parser Options
optionsParser flows =
  QueryOptions <$> optional dayParser <*> summarizeParser <*> many groupParser
    <|> RecordOptions <$> flowTypeParser flows
    <|> NotesOptions <$> dayParser <* notesParser <*> formatParser

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

parseSenseiOptions ::
  UserProfile -> IO Options
parseSenseiOptions UserProfile{userFlowTypes} = execParser (optionsParserInfo userFlowTypes)

display :: ToJSON a => a -> IO ()
display = LBS.putStr . encode

flowAction :: Options -> Text -> UTCTime -> Text -> IO ()
flowAction (QueryOptions Nothing False grps) usrName _ _ =
  send (queryFlowC usrName grps) >>= display
flowAction (QueryOptions Nothing True _) usrName _ _ =
  send (queryFlowSummaryC usrName) >>= display
flowAction (QueryOptions (Just day) False _) usrName _ _ =
  send (queryFlowDayC usrName day) >>= display
flowAction (QueryOptions (Just day) True _) usrName _ _ =
  send (queryFlowDaySummaryC usrName day) >>= display
flowAction (NotesOptions day noteFormat) usrName _ _ =
  send (notesDayC usrName day) >>= mapM_ println . fmap encodeUtf8 . formatNotes noteFormat
flowAction (RecordOptions ftype) curUser startDate curDir =
  case ftype of
    Note -> do
      txt <- captureNote
      send $ flowC curUser Note (FlowNote curUser startDate curDir txt)
    other ->
      send $ flowC curUser other (FlowState curUser startDate curDir)

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

captureNote :: IO Text.Text
captureNote = do
  setSGR
    [ SetConsoleIntensity NormalIntensity,
      SetColor Foreground Vivid White,
      SetColor Background Dull Blue
    ]
  putStr "Record a note, type Ctrl+D at beginning of line when done"
  setSGR [Reset]
  putStrLn ""
  capture []
  where
    capture :: [Text.Text] -> IO Text.Text
    capture acc = do
      eof <- isEOF
      if eof
        then pure $ Text.unlines (reverse acc)
        else do
          res <- Exc.try $ hGetLine stdin
          case res of
            Left (_ex :: Exc.IOException) -> pure $ Text.unlines (reverse acc)
            Right txt -> capture (Text.pack txt : acc)
