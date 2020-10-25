{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Exception.Safe as Exc
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import Data.Time
import Data.Time.Format.ISO8601
import Options.Applicative
import Sensei.API
import Sensei.App
import Sensei.Client
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.User
import System.Process

usage :: IO ()
usage =
  mapM_ putStrLn $
    [ "Usage: ep <flow type>",
      "Record start time of some flow type for current user",
      "Arguments:",
      "  <flow type> : One of l(earning), e(xperimenting), t(troubleshooting), f(lowing), r(ework), o(ther)"
    ]

data Options
  = QueryOptions {queryDay :: Maybe Day, summarize :: Bool}
  | RecordOptions {recordType :: FlowType}

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    (progDesc "Eopché")

optionsParser :: Parser Options
optionsParser =
  QueryOptions <$> optional dayParser <*> summarizeParser
    <|> RecordOptions <$> flowTypeParser

dayParser :: Parser Day
dayParser =
  option
    (maybeReader iso8601ParseM)
    ( long "date"
        <> short 'd'
        <> metavar "DATE"
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

flowTypeParser :: Parser FlowType
flowTypeParser =
  flag' Experimenting (short 'e' <> help "Experimenting period")
    <|> flag' Learning (short 'l' <> help "Learning period")
    <|> flag' Troubleshooting (short 't' <> help "Troubleshooting period")
    <|> flag' Flowing (short 'f' <> help "Flowing period")
    <|> flag' Rework (short 'r' <> help "Rework period")
    <|> flag' Other (short 'o' <> help "Other period")
    <|> flag' Note (short 'n' <> help "Taking some note")

display :: ToJSON a => a -> IO ()
display = Text.putStrLn . decodeUtf8 . LBS.toStrict . encode

recordFlow :: Options -> String -> UTCTime -> FilePath -> IO ()
recordFlow (QueryOptions Nothing _) userName _ _ =
  send (queryFlowC userName) >>= display
recordFlow (QueryOptions (Just day) False) userName _ _ =
  send (queryFlowDayC userName day) >>= display
recordFlow (QueryOptions (Just day) True) userName _ _ =
  send (queryFlowDaySummaryC userName day) >>= display
recordFlow (RecordOptions ftype) curUser startDate curDir =
  case ftype of
    Note -> do
      txt <- captureNote
      send $ flowC Note (FlowNote curUser startDate curDir txt)
    other ->
      send $ flowC other (FlowState curUser startDate curDir)

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

wrapProg :: [Char] -> [String] -> UTCTime -> FilePath -> IO ()
wrapProg realProg progArgs st currentDir = do
  (_, _, _, procHandle) <-
    createProcess
      (proc realProg progArgs)
        { std_in = Inherit,
          std_out = Inherit,
          std_err = Inherit
        }

  ex <- waitForProcess procHandle
  en <- getCurrentTime
  send (traceC $ Trace en currentDir realProg progArgs ex (diffUTCTime en st))
  exitWith ex

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- getCurrentTime
  curUser <- getLoginName

  case prog of
    "git" -> wrapProg "/usr/bin/git" progArgs st currentDir
    "stak" -> wrapProg (homeDir </> ".local/bin/stack") progArgs st currentDir
    "docker" -> wrapProg "/usr/local/bin/docker" progArgs st currentDir
    "ep" -> do
      opts <- execParser optionsParserInfo
      recordFlow opts curUser st currentDir
    "sensei-exe" -> startServer
    _ -> error $ "Don't know how to handle program " <> prog
