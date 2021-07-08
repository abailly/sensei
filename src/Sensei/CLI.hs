{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI where

import Data.Aeson hiding (Options, Success)
import Data.Aeson.Encode.Pretty (encodePretty)
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
import Sensei.IO (getConfigDirectory, writeConfig)
import Sensei.Server.Auth.Types (createKeys, createToken, getPublicKey, setPassword)
import Sensei.Version
import Servant (Headers (getResponse))
import System.Exit
import System.IO
import System.IO.Unsafe

data Options
  = QueryOptions QueryOptions
  | RecordOptions RecordOptions
  | NotesOptions {notesQuery :: NotesQuery, format :: NoteFormat}
  | UserOptions {userAction :: UserAction}
  | AuthOptions AuthOptions
  | CommandOptions CommandOptions
  deriving (Show, Eq)

data QueryOptions
  = FlowQuery {queryDay :: Day, summarize :: Bool, groups :: [Group]}
  | GetAllLogs
  deriving (Eq, Show)

data RecordOptions
  = SingleFlow {recordType :: FlowType}
  | FromFile FilePath
  deriving (Eq, Show)

data NotesQuery = QueryDay Day | QuerySearch Text
  deriving (Show, Eq)

data UserAction
  = GetProfile
  | SetProfile {profileFile :: FilePath}
  | GetVersions
  | ShiftTimestamp TimeDifference
  | GetFlow Reference
  deriving (Show, Eq)

data AuthOptions
  = CreateKeys
  | CreateToken
  | SetPassword
  | PublicKey
  | NoOp
  deriving (Show, Eq)

data CommandOptions = Command { exe :: String, args :: [String] }
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
    (commandsParser flows <**> helper)
    ( progDesc $
        unlines
          [ "ep(oché) - Record and query all kind of coding activity: flows, commands, notes",
            "version: " <> showVersion senseiVersion <> ", storage: " <> show currentVersion
          ]
    )

commandsParser :: Maybe [FlowType] -> Parser Options
commandsParser flows =
  hsubparser
    ( command "auth" (info authOptions (progDesc "Manage authentication keys and tokens"))
        <> command "query" (info queryOptions (progDesc "Query data and summaries"))
        <> command "record" (info (recordOptions flows) (progDesc "Record flows"))
        <> command "notes" (info notesOptions (progDesc "Record and query notes"))
        <> command "user" (info userOptions (progDesc "Get and set user profile"))
        <> command "command" (info commandOptions (progDesc "Wrap and record arbitrary commands execution"))
    )

authOptions :: Parser Options
authOptions =
  AuthOptions <$> (createKeysParser <|> publicKeyParser <|> createTokenParser <|> setPasswordParser)

queryOptions :: Parser Options
queryOptions =
  QueryOptions
    <$> ( FlowQuery <$> dayParser <*> summarizeParser <*> many groupParser
            <|> pure GetAllLogs <* allLogsParser
        )

recordOptions :: Maybe [FlowType] -> Parser Options
recordOptions flows =
  RecordOptions
    <$> ( SingleFlow <$> flowTypeParser flows
            <|> FromFile <$> fromFileParser
        )

notesOptions :: Parser Options
notesOptions = NotesOptions <$> (notesParser *> ((QueryDay <$> dayParser) <|> searchParser)) <*> formatParser

userOptions :: Parser Options
userOptions = UserOptions <$> (profileParser *> userActionParser)

commandOptions :: Parser Options
commandOptions = CommandOptions <$> commandParser

{-# NOINLINE today #-}
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

searchParser :: Parser NotesQuery
searchParser =
  QuerySearch . Text.pack
    <$> strOption
      ( long "search"
          <> short 's'
          <> metavar "TEXT"
          <> help "full-text query to search notes."
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

allLogsParser :: Parser ()
allLogsParser =
  flag
    ()
    ()
    ( long "all-logs"
        <> short 'l'
        <> help "retrieve the full log for the current user"
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

fromFileParser :: Parser FilePath
fromFileParser =
  strOption
    ( long "from-file"
        <> help "Read list of events to post from given JSON file"
    )

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

createKeysParser :: Parser AuthOptions
createKeysParser =
  flag
    NoOp
    CreateKeys
    ( long "create-keys"
        <> short 'c'
        <> help "Create a new pair of keys, storing them in user's config directory"
    )

publicKeyParser :: Parser AuthOptions
publicKeyParser =
  flag
    NoOp
    PublicKey
    ( long "public-key"
        <> short 'k'
        <> help
          "Output a JWK representation of the public key from existing private key. \
          \ This is useful for generating a public key from private key to put on a server."
    )

createTokenParser :: Parser AuthOptions
createTokenParser =
  flag
    NoOp
    CreateToken
    ( long "create-token"
        <> short 't'
        <> help "Create a new token using existing key, and update 'client.json' configuration file"
    )

setPasswordParser :: Parser AuthOptions
setPasswordParser =
  flag
    NoOp
    SetPassword
    ( long "set-password"
        <> short 'p'
        <> help "Sets the password for the current user in his server's profile. Password is read from stdin."
    )

commandParser :: Parser CommandOptions
commandParser =
  Command <$>
  strArgument (help "command name") <*>
  many (strArgument (help "command argument(s)"))

parseSenseiOptions ::
  Maybe [FlowType] -> IO Options
parseSenseiOptions flows = execParser (optionsParserInfo flows)

display :: ToJSON a => a -> IO ()
display = LBS.putStr . encodePretty

ep :: ClientConfig -> Options -> Text -> UTCTime -> Text -> IO ()
ep config (QueryOptions (FlowQuery day False [])) usrName _ _ =
  send config (queryFlowDayC usrName day) >>= display
ep config (QueryOptions (FlowQuery day True [])) usrName _ _ =
  send config (queryFlowPeriodSummaryC usrName (Just day) (Just $ succ day) Nothing) >>= display . getResponse
ep config (QueryOptions (FlowQuery _ _ grps)) usrName _ _ =
  send config (queryFlowC usrName grps) >>= display
ep config (QueryOptions GetAllLogs) usrName _ _ =
  send config (getLogC usrName Nothing) >>= display . getResponse
ep config (NotesOptions (QueryDay day) noteFormat) usrName _ _ =
  send config (notesDayC usrName day) >>= mapM_ println . fmap encodeUtf8 . formatNotes noteFormat . getResponse
ep config (NotesOptions (QuerySearch txt) noteFormat) usrName _ _ =
  send config (searchNotesC usrName (Just txt)) >>= mapM_ println . fmap encodeUtf8 . formatNotes noteFormat
ep config (RecordOptions (SingleFlow ftype)) curUser startDate curDir =
  case ftype of
    Note -> do
      txt <- captureNote
      send config $ postEventC [EventNote $ NoteFlow curUser startDate curDir txt]
    _ ->
      send config $ postEventC [EventFlow $ Flow ftype curUser startDate curDir]
ep config (RecordOptions (FromFile fileName)) _ _ _ = do
  decoded <- eitherDecode <$> LBS.readFile fileName
  case decoded of
    Left err -> hPutStrLn stderr ("failed to decode events from " <> fileName <> ": " <> err) >> exitWith (ExitFailure 1)
    Right events -> do
      let chunks [] = []
          chunks xs =
            let (chunk, rest) = splitAt 50 xs
             in chunk : chunks rest
      mapM_
        ( \evs -> do
            putStrLn $ "Sending " <> show (length evs) <> " events: " <> show evs
            send config $ postEventC evs
        )
        $ chunks events
ep config (UserOptions GetProfile) usrName _ _ =
  send config (getUserProfileC usrName) >>= display
ep config (UserOptions (SetProfile file)) usrName _ _ = do
  profile <- eitherDecode <$> LBS.readFile file
  case profile of
    Left err -> hPutStrLn stderr ("failed to decode user profile from " <> file <> ": " <> err) >> exitWith (ExitFailure 1)
    Right prof -> void $ send config (setUserProfileC usrName prof)
ep config (UserOptions GetVersions) _ _ _ = do
  vs <- send config getVersionsC
  display vs {clientVersion = senseiVersion, clientStorageVersion = currentVersion}
ep config (UserOptions (ShiftTimestamp diff)) curUser _ _ =
  send config (updateFlowC curUser diff) >>= display
ep config (UserOptions (GetFlow q)) curUser _ _ =
  send config (getFlowC curUser q) >>= display
ep _config (AuthOptions CreateKeys) _ _ _ = getConfigDirectory >>= createKeys
ep _config (AuthOptions PublicKey) _ _ _ = getConfigDirectory >>= getPublicKey >>= display
ep config (AuthOptions CreateToken) _ _ _ = do
  token <- getConfigDirectory >>= createToken
  writeConfig config {authToken = Just token}
ep config (AuthOptions SetPassword) userName _ _ = do
  oldProfile <- send config (getUserProfileC userName)
  pwd <- readPassword
  newProfile <- setPassword oldProfile pwd
  void $ send config (setUserProfileC userName newProfile)
ep _config (AuthOptions NoOp) _ _ _ = pure ()
ep _config (CommandOptions _) _ _ _ = pure ()

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
