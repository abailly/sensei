{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Main where

import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.Server
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Posix.Daemonize
import           System.Process

-- * API
type SenseiAPI = "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

-- * Client
traceC :: Trace -> ClientM ()
traceC = client senseiAPI

-- * Server
traceS :: Handle -> Trace -> Handler ()
traceS out trace = liftIO $ do
  LBS.hPutStr out $ encode trace <> "\n"
  hFlush out

sensei :: FilePath -> IO ()
sensei output = do
  hdl <- openBinaryFile output AppendMode
  run 23456 (serve senseiAPI $ traceS hdl)


data Trace = Trace { timestamp :: UTCTime
                   , directory :: FilePath
                   , process   :: String
                   , args      :: [ String ]
                   , exit_code :: ExitCode
                   , elapsed   :: NominalDiffTime
                   }
           deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode
deriving instance FromJSON ExitCode

send :: Trace -> IO ()
send trace = do
  mgr <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "127.0.0.1"  23456 ""
  res <- runClientM (traceC trace) (ClientEnv mgr baseUrl)
  case res of
    -- server is not running, fork it
    Left (ConnectionError _) -> do
      homeDir <- getHomeDirectory
      let senseiLog = homeDir </> ".sensei.log"
      daemonize $ sensei senseiLog
    Right () -> pure ()

main :: IO ()
main = do
  homeDir    <- getHomeDirectory
  currentDir <- getCurrentDirectory
  prog       <- getProgName
  args       <- getArgs
  st         <- getCurrentTime

  hSetBuffering stdin NoBuffering

  let realProg = case prog of
        "git"    -> "/usr/bin/git"
        "stak"   -> homeDir </> ".local/bin/stack"
        "docker" -> "/usr/local/bin/docker"
        _        -> error $ "Don't know how to handle program " <> prog

  (_, _, _, procHandle) <- createProcess
                                   (proc realProg args)
                                   { std_in = Inherit, std_out = Inherit, std_err = Inherit }

  ex <- waitForProcess procHandle
  en <- getCurrentTime
  send (Trace en currentDir realProg args ex (diffUTCTime en st))
  exitWith ex
