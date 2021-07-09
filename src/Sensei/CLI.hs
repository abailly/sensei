{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI
  ( -- * Options
    Options (..),
    RecordOptions (..),
    QueryOptions (..),
    UserOptions (..),
    NotesOptions (..),
    NotesQuery (..),
    AuthOptions (..),
    CommandOptions (..),
    runOptionsParser,
    parseSenseiOptions,

    -- * Main entrypoint
    ep,
  )
where

import Data.Aeson hiding (Options, Success)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Sensei.API
import Sensei.CLI.Options
import Sensei.CLI.Terminal
import Sensei.Client
import Sensei.IO (getConfigDirectory, writeConfig)
import Sensei.Server.Auth.Types (createKeys, createToken, getPublicKey, setPassword, Credentials(..))
import Sensei.Version
import Sensei.Wrapper (handleWrapperResult, tryWrapProg, wrapperIO)
import Servant (Headers (getResponse))
import System.Exit
import System.IO

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
ep config (NotesOptions (NotesQuery (QueryDay day) noteFormat)) usrName _ _ =
  send config (notesDayC usrName day) >>= mapM_ println . fmap encodeUtf8 . formatNotes noteFormat . getResponse
ep config (NotesOptions (NotesQuery (QuerySearch txt) noteFormat)) usrName _ _ =
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
ep config (AuthOptions GetToken) userName _ _ = do
  pwd <- readPassword
  token <- send config $ do
    -- authenticate with password
    void $ loginC $ Credentials userName pwd
    getFreshTokenC userName
  writeConfig config {authToken = Just token}
ep _config (AuthOptions NoOp) _ _ _ = pure ()
ep config (CommandOptions (Command exe args)) userName _ currentDir = do
  let io = wrapperIO config
  tryWrapProg io userName exe args currentDir >>= handleWrapperResult exe

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
