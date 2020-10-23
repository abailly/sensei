{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Control.Exception.Safe as Exc
import Control.Monad.Trans
import Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Options.Applicative
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Daemonize
import System.Posix.User
import System.Process

-- * Doc

usage :: IO ()
usage =
  mapM_ putStrLn $
    [ "Usage: ep <flow type>",
      "Record start time of some flow type for current user",
      "Arguments:",
      "  <flow type> : One of l(earning), e(xperimenting), t(troubleshooting), f(lowing), r(ework), o(ther)"
    ]

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flow"
      :> ( Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()
             :<|> Capture "user" String :> Capture "day" Day :> Get '[JSON] [FlowView]
             :<|> Capture "user" String :> Get '[JSON] [FlowView]
         )

-- | An ordered sequence of flows for a given user
data FlowView = FlowView
  { flowStart :: UTCTime,
    flowEnd :: UTCTime,
    duration :: NominalDiffTime,
    flowType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Rework | Other
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
  parseUrlPiece "Rework" = pure Rework
  parseUrlPiece _txt = pure Other

data FlowState = FlowState
  { _flowUser :: String,
    _flowStart :: UTCTime,
    _flowDir :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- * Client

traceC :: Trace -> ClientM ()
flowC :: FlowType -> FlowState -> ClientM ()
queryFlowC :: String -> ClientM [FlowView]
queryFlowDayC :: String -> Day -> ClientM [FlowView]
traceC :<|> (flowC :<|> queryFlowDayC :<|> queryFlowC) = client senseiAPI

-- * Server

traceS :: FilePath -> Trace -> Handler ()
traceS file trace = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode trace <> "\n"
    hFlush out

flowS :: FilePath -> FlowType -> FlowState -> Handler ()
flowS file flowTyp flow = liftIO $
  withBinaryFile file AppendMode $ \out -> do
    LBS.hPutStr out $ encode (Flow flowTyp flow) <> "\n"
    hFlush out

queryFlowDayS :: FilePath -> [Char] -> Day -> Handler [FlowView]
queryFlowDayS file usr _day =
  liftIO $ withBinaryFile file ReadMode $ \hdl -> liftIO $ loop hdl usr []

queryFlowS :: FilePath -> [Char] -> Handler [FlowView]
queryFlowS file usr =
  liftIO $ withBinaryFile file ReadMode $ \hdl -> liftIO $ loop hdl usr []

loop :: Handle -> String -> [FlowView] -> IO [FlowView]
loop hdl usr acc = do
  res <- Exc.try $ liftIO $ LT.hGetLine hdl
  case res of
    Left (_ex :: Exc.IOException) -> pure (reverse acc)
    Right ln ->
      case eitherDecode (LT.encodeUtf8 ln) of
        Left _err -> loop hdl usr acc
        Right flow -> loop hdl usr (flowView flow usr acc)

flowView :: Flow -> String -> [FlowView] -> [FlowView]
flowView Flow {..} usr views =
  if _flowUser _flowState == usr
    then
      let view = FlowView st st 0 _flowType
          st = _flowStart _flowState
       in case views of
            (v : vs) -> view : v {flowEnd = st, duration = diffUTCTime st (flowStart v)} : vs
            [] -> [view]
    else views

sensei :: FilePath -> IO ()
sensei output =
  run 23456 (serve senseiAPI $ traceS output :<|> (flowS output :<|> queryFlowDayS output :<|> queryFlowS output))

data Trace = Trace
  { timestamp :: UTCTime,
    directory :: FilePath,
    process :: String,
    args :: [String],
    exit_code :: ExitCode,
    elapsed :: NominalDiffTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode

deriving instance FromJSON ExitCode

daemonizeServer :: IO ()
daemonizeServer = do
  homeDir <- getHomeDirectory
  let senseiLog = homeDir </> ".sensei.log"
  daemonize $ sensei senseiLog

startServer :: IO ()
startServer = do
  homeDir <- getHomeDirectory
  let senseiLog = homeDir </> ".sensei.log"
  sensei senseiLog

send :: ClientM a -> IO a
send act = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http "127.0.0.1" 23456 ""
  res <- runClientM act (ClientEnv mgr base Nothing)
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      daemonizeServer
      -- retry sending the trace to server
      send act
    -- something is wrong, bail out
    Left otherError -> error $ "failed to connect to server: " <> show otherError
    Right v -> pure v

-- * Command-line Actions

data Options =
  QueryOptions { queryDay :: Maybe Day }

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    (progDesc "Eopché")

optionsParser :: Parser Options
optionsParser =
  QueryOptions <$> optional dayParser

dayParser :: Parser Day
dayParser =
  option
    (maybeReader iso8601ParseM)
    ( long "date"
        <> short 'd'
        <> metavar "DATE"
        <> help "date (default: 8899)"
    )


queryFlow :: Options -> String -> IO ()
queryFlow (QueryOptions Nothing) userName =
  send (queryFlowC userName) >>= Text.putStrLn . decodeUtf8 . LBS.toStrict . encode
queryFlow (QueryOptions (Just day)) userName = do
  views <- filter (sameDayThan day) <$> send (queryFlowDayC userName day)
  Text.putStrLn . decodeUtf8 . LBS.toStrict . encode $ views

sameDayThan :: Day -> FlowView -> Bool
sameDayThan day FlowView{flowStart} =
  utctDay flowStart == day

recordFlow :: [String] -> String -> UTCTime -> FilePath -> IO ()
recordFlow [] _ _ _ = pure ()
recordFlow (ftype:_) curUser startDate curDir =
  send $ flowC (parseFlowType ftype) (FlowState curUser startDate curDir)
  where
    parseFlowType "e" = Experimenting
    parseFlowType "l" = Learning
    parseFlowType "t" = Troubleshooting
    parseFlowType "f" = Flowing
    parseFlowType "r" = Rework
    parseFlowType _ = Other

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
  homeDir <- getHomeDirectory
  currentDir <- getCurrentDirectory
  prog <- getProgName
  progArgs <- getArgs
  st <- getCurrentTime
  curUser <- getLoginName

  hSetBuffering stdin NoBuffering

  case prog of
    "git" -> wrapProg "/usr/bin/git" progArgs st currentDir
    "stak" -> wrapProg (homeDir </> ".local/bin/stack") progArgs st currentDir
    "docker" -> wrapProg "/usr/local/bin/docker" progArgs st currentDir
    "epq" -> do
      opts <- execParser optionsParserInfo
      queryFlow opts curUser
    "ep" -> recordFlow progArgs curUser st currentDir
    "sensei-exe" -> startServer
    _ -> error $ "Don't know how to handle program " <> prog
