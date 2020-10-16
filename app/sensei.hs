{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Time
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
usage = mapM_ putStrLn $ [
  "Usage: ep <flow type>",
  "Record start time of some flow type for current user",
  "Arguments:",
  "  <flow type> : One of l(earning), e(xperimenting), t(troubleshooting), f(lowing), o(ther)"
  ]

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flow" :> Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Other
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
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
traceC :<|> flowC = client senseiAPI

-- * Server

traceS :: Handle -> Trace -> Handler ()
traceS out trace = liftIO $ do
  LBS.hPutStr out $ encode trace <> "\n"
  hFlush out

flowS :: Handle -> FlowType -> FlowState -> Handler ()
flowS out flowType flow = liftIO $ do
  LBS.hPutStr out $ encode (Flow flowType flow) <> "\n"
  hFlush out

sensei :: FilePath -> IO ()
sensei output = do
  hdl <- openBinaryFile output AppendMode
  run 23456 (serve senseiAPI $ traceS hdl :<|> flowS hdl)

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

send :: ClientM () -> IO ()
send act = do
  mgr <- newManager defaultManagerSettings
  let base = BaseUrl Http "127.0.0.1" 23456 ""
  res <- runClientM act (ClientEnv mgr base Nothing)
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      homeDir <- getHomeDirectory
      let senseiLog = homeDir </> ".sensei.log"
      daemonize $ sensei senseiLog
      -- retry sending the trace to server
      send act
    -- something is wrong, bail out
    Left otherError -> error $ "failed to connect to server: " <> show otherError
    Right () -> pure ()

-- * Command-line Actions

recordFlow :: [String] -> String -> UTCTime -> FilePath -> IO ()
recordFlow [] _ _ _ = do
  usage
  exitWith (ExitFailure 1)
recordFlow (ftype:_) curUser st curDir =
  send $ flowC (parseFlowType ftype) (FlowState curUser st curDir)
  where
    parseFlowType "e" = Experimenting
    parseFlowType "l" = Learning
    parseFlowType "t" = Troubleshooting
    parseFlowType "f" = Flowing
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
    "stak" -> wrapProg (homeDir </> ".local/bin/stack")  progArgs st currentDir
    "docker" -> wrapProg "/usr/local/bin/docker"  progArgs st currentDir
    "ep" -> recordFlow progArgs curUser st currentDir
    _ -> error $ "Don't know how to handle program " <> prog
