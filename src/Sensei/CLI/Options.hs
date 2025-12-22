{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI.Options where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (
  Day,
  LocalTime (localDay),
  UTCTime (UTCTime),
  ZonedTime (zonedTimeToLocalTime),
  getZonedTime,
 )
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Options.Applicative (
  Alternative (many, (<|>)),
  Parser,
  ParserFailure (execFailure),
  ParserInfo,
  ParserResult (Failure, Success),
  auto,
  command,
  defaultPrefs,
  eitherReader,
  execParser,
  execParserPure,
  flag,
  flag',
  help,
  helper,
  hsubparser,
  info,
  long,
  maybeReader,
  metavar,
  optional,
  option,
  progDesc,
  short,
  strArgument,
  strOption,
  value,
  (<**>),
 )
import Options.Applicative.Help (renderHelp)
import Sensei.Duration (TimeDifference, parse)
import Sensei.Flow (
  FlowType (..),
  NoteFormat,
  Reference (Latest),
  defaultFlowTypes,
  parseNoteFormat,
  parseRef,
 )
import Sensei.Graph (
  Op,
  add,
  done,
  goal,
  link,
  pop,
  push,
  remove,
  shift,
 )
import Sensei.Group (Group)
import Sensei.Version (
  currentVersion,
  senseiVersion,
  showVersion,
 )
import System.IO.Unsafe (unsafePerformIO)

data Options
  = QueryOptions QueryOptions
  | RecordOptions RecordOptions
  | NotesOptions NotesOptions
  | UserOptions UserOptions
  | AuthOptions AuthOptions
  | CommandOptions CommandOptions
  | GoalOptions GoalOptions
  | ArticleOptions ArticleOptions
  deriving (Show, Eq)

data QueryOptions
  = FlowQuery {queryDay :: Day, summarize :: Bool, groups :: [Group]}
  | GetAllLogs
  | ShiftTimestamp TimeDifference
  | GetFlow Reference
  deriving (Eq, Show)

data RecordOptions
  = SingleFlow {recordType :: FlowType}
  | FromFile FilePath
  deriving (Eq, Show)

data NotesOptions = NotesQuery {notesQuery :: NotesQuery, format :: NoteFormat}
  deriving (Show, Eq)

data NotesQuery = QueryDay Day | QuerySearch Text
  deriving (Show, Eq)

data UserOptions
  = GetProfile
  | SetProfile {profileFile :: FilePath}
  | GetVersions
  deriving (Show, Eq)

data AuthOptions
  = CreateKeys
  | CreateToken
  | GetToken
  | SetPassword
  | EncryptPassword
  | PublicKey
  | NoOp
  deriving (Show, Eq)

data CommandOptions = Command {exe :: String, args :: [String]}
  deriving (Show, Eq)

data GoalOptions = GetGraph | UpdateGraph Op
  deriving (Show, Eq)

data ArticleOptions
  = PublishArticle
      { articleFile :: FilePath
      , articleDate :: Maybe UTCTime
      }
  | UpdateArticle
      { updateKey :: Text
      , updateFile :: FilePath
      , updateDate :: Maybe UTCTime
      }
  | DeleteArticle
      { deleteKey :: Text
      }
  | ListArticles
  deriving (Show, Eq)

runOptionsParser ::
  Maybe [FlowType] -> [String] -> Either Text Options
runOptionsParser flows arguments =
  case execParserPure defaultPrefs (optionsParserInfo flows) arguments of
    Success opts -> Right opts
    Failure f -> Left (Text.pack . Options.Applicative.Help.renderHelp 80 . fst3 $ execFailure f "")
     where
      fst3 (a, _, _) = a
    _ -> Left "Unexpected completion invoked"

optionsParserInfo :: Maybe [FlowType] -> ParserInfo Options
optionsParserInfo flows =
  info
    (commandsParser flows <**> helper)
    ( progDesc $
        unlines
          [ "ep(oché) - Record and query all kind of coding activity: flows, commands, notes"
          , "version: " <> showVersion senseiVersion <> ", storage: " <> show currentVersion
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
        <> command
          "command"
          ( info
              commandOptions
              ( progDesc
                  "Wrap and record arbitrary commands execution. To wrap any command, \
                  \ pass '--' after the 'command' word, then the command executable \
                  \ and the args."
              )
          )
        <> command "goal" (info goalOptions (progDesc "Define and manipulate goals graph"))
        <> command "article" (info articleOptions (progDesc "Publish articles to Bluesky"))
    )

authOptions :: Parser Options
authOptions =
  AuthOptions
    <$> ( createKeysParser
            <|> publicKeyParser
            <|> createTokenParser
            <|> getTokenParser
            <|> setPasswordParser
            <|> encryptPasswordParser
        )

queryOptions :: Parser Options
queryOptions =
  QueryOptions
    <$> ( FlowQuery
            <$> dayParser
            <*> summarizeParser
            <*> many groupParser
            <|> pure GetAllLogs
              <* allLogsParser
            <|> ( ShiftTimestamp
                    <$> option
                      (eitherReader parse)
                      ( long "shift-timestamp"
                          <> short 'S'
                          <> help "shift the latest flow by the given time difference"
                      )
                )
            <|> ( GetFlow
                    <$> option
                      (eitherReader parseRef)
                      ( long "get-flow"
                          <> short 'F'
                          <> value Latest
                          <> help "Query some flow or group of FlowViews from underlying storage (default: latest)"
                      )
                )
        )

recordOptions :: Maybe [FlowType] -> Parser Options
recordOptions flows =
  RecordOptions
    <$> ( SingleFlow
            <$> flowTypeParser flows
            <|> FromFile
              <$> fromFileParser
        )

notesOptions :: Parser Options
notesOptions = NotesOptions <$> (NotesQuery <$> (notesParser *> ((QueryDay <$> dayParser) <|> searchParser)) <*> formatParser)

userOptions :: Parser Options
userOptions = UserOptions <$> (profileParser *> userActionParser)

commandOptions :: Parser Options
commandOptions = CommandOptions <$> commandParser

goalOptions :: Parser Options
goalOptions = GoalOptions <$> goalParser

articleOptions :: Parser Options
articleOptions = ArticleOptions <$> articleParser

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

userActionParser :: Parser UserOptions
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

getTokenParser :: Parser AuthOptions
getTokenParser =
  flag
    NoOp
    GetToken
    ( long "get-token"
        <> short 'g'
        <> help "Retrieve a fresh token from the server, authenticating user with password, and update 'client.json' configuration file"
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

encryptPasswordParser :: Parser AuthOptions
encryptPasswordParser =
  flag
    NoOp
    EncryptPassword
    ( long "encrypt-password"
        <> short 'e'
        <> help "Encrypt the given password and outputs the result to stdout. Password is read from stdin."
    )

commandParser :: Parser CommandOptions
commandParser =
  Command
    <$> strArgument (help "command name")
    <*> many (strArgument (help "command argument(s)"))

goalParser :: Parser GoalOptions
goalParser =
  ( UpdateGraph
      <$> ( pushGoalParser
              <|> popGoalParser
              <|> doneGoalParser
              <|> shiftGoalParser
              <|> setGoalParser
              <|> addGoalParser
              <|> linkGoalParser
              <|> removeGoalParser
          )
  )
    <|> pure GetGraph
 where
  pushGoalParser =
    flag'
      push
      ( long "push"
          <> short 'p'
          <> help "Set children of current goal(s) as current"
      )

  popGoalParser =
    flag'
      pop
      ( long "pop"
          <> short 'P'
          <> help "Set parent(s) of current goal(s) as current"
      )

  shiftGoalParser =
    flag'
      shift
      ( long "shift"
          <> short 's'
          <> help "Set children of parent of current goal as current"
      )

  doneGoalParser =
    flag'
      done
      ( long "done"
          <> short 'd'
          <> help "Mark first current goal as done, setting parents as current if all done"
      )

  setGoalParser =
    goal
      <$> strOption
        ( long "set-goal"
            <> short 'g'
            <> help "Set given goal as a children of current goal(s)"
        )

  addGoalParser =
    add
      <$> strOption
        ( long "add-goal"
            <> short 'a'
            <> help "Mark current goal as done and set given goal as its parent"
        )

  removeGoalParser =
    remove
      <$> strOption
        ( long "remove-goal"
            <> short 'r'
            <> help "Remove current goal and its children from the graph"
        )

  linkGoalParser =
    link
      <$> strOption
        ( long "link"
            <> short 'l'
            <> help "Link 2 goals"
        )
      <*> strArgument (help "Target")

parseDateOrDateTime :: String -> Maybe UTCTime
parseDateOrDateTime s =
  -- Try date-only format first (YYYY-MM-DD) and convert to midnight UTC
  (parseTimeM True defaultTimeLocale "%Y-%m-%d" s >>= \day -> Just $ UTCTime day 0)
    <|> -- Fall back to full ISO8601 format
      iso8601ParseM s

publishArticleParser :: Parser ArticleOptions
publishArticleParser =
  PublishArticle
    <$> strOption
      ( long "publish"
          <> short 'a'
          <> metavar "FILE"
          <> help "Publish article from given file to Bluesky"
      )
    <*> optional
      ( option
          (maybeReader parseDateOrDateTime)
          ( long "date"
              <> short 'd'
              <> metavar "ISO8601_DATE"
              <> help "Publication date (ISO8601 format: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSZ)"
          )
      )

updateArticleParser :: Parser ArticleOptions
updateArticleParser =
  UpdateArticle
    <$> (Text.pack <$> strOption
          ( long "update"
              <> short 'u'
              <> metavar "TID"
              <> help "Update existing article by TID (e.g., '3jzfcijpj2z2a')"
          ))
    <*> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "Path to the updated article content"
      )
    <*> optional
      ( option
          (maybeReader parseDateOrDateTime)
          ( long "date"
              <> short 'd'
              <> metavar "ISO8601_DATE"
              <> help "Update date (ISO8601 format: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SSZ)"
          )
      )

deleteArticleParser :: Parser ArticleOptions
deleteArticleParser =
  DeleteArticle
    <$> (Text.pack <$> strOption
          ( long "delete"
              <> short 'D'
              <> metavar "TID"
              <> help "Delete article by TID (e.g., '3jzfcijpj2z2a')"
          ))

listArticlesParser :: Parser ArticleOptions
listArticlesParser =
  flag'
    ListArticles
    ( long "list"
        <> short 'l'
        <> help "List all published articles"
    )

articleParser :: Parser ArticleOptions
articleParser =
  publishArticleParser
    <|> updateArticleParser
    <|> deleteArticleParser
    <|> listArticlesParser

parseSenseiOptions ::
  Maybe [FlowType] -> IO Options
parseSenseiOptions flows = execParser (optionsParserInfo flows)
