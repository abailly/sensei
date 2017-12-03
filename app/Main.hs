{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid
import           Data.Time
import           GHC.Generics
import           Lib
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

data Trace = Trace { timestamp :: UTCTime
                   , process   :: String
                   , args      :: [ String ]
                   , exit_code :: ExitCode
                   , elapsed   :: NominalDiffTime
                   }
           deriving (Eq, Show, Generic, ToJSON, FromJSON)

deriving instance ToJSON ExitCode
deriving instance FromJSON ExitCode

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  st <- getCurrentTime

  let realProg = case prog of
        "git" -> "/usr/bin/git"
        _     -> error $ "Don't know how to handle program " <> prog

  (_, _, _, procHandle) <- createProcess
                                   (proc realProg args)
                                   { std_in = Inherit, std_out = Inherit, std_err = Inherit, create_group = True }

  ex <- waitForProcess procHandle
  en <- getCurrentTime
  LBS.hPutStr stdout $ encode (Trace en prog args ex (diffUTCTime en st)) <> "\n"
  exitWith ex
