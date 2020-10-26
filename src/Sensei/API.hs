{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.API where

import Data.Aeson hiding (Options)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics
import Servant
import System.Exit

-- * API

type SenseiAPI =
  "trace" :> ReqBody '[JSON] Trace :> Post '[JSON] ()
    :<|> "flows"
      :> ( Capture "flowType" FlowType :> ReqBody '[JSON] FlowState :> Post '[JSON] ()
             :<|> Capture "user" String :> Capture "day" Day :> "summary" :> Get '[JSON] [(FlowType, NominalDiffTime)]
             :<|> Capture "user" String :> Capture "day" Day :> "notes" :> Get '[JSON] [(UTCTime,Text.Text)]
             :<|> Capture "user" String :> Capture "day" Day :> Get '[JSON] [FlowView]
             :<|> Capture "user" String :> Get '[JSON] [FlowView]
         )
    :<|> Raw

-- | Execution "trace" of a program
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

-- | A single flow for a given user
data FlowView = FlowView
  { flowStart :: UTCTime,
    flowEnd :: UTCTime,
    duration :: NominalDiffTime,
    flowType :: FlowType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

sameDayThan :: Day -> (a -> UTCTime) -> a -> Bool
sameDayThan day selector a =
  utctDay (selector a) == day

senseiAPI :: Proxy SenseiAPI
senseiAPI = Proxy

data FlowType = Learning | Experimenting | Troubleshooting | Flowing | Rework | Note | Other
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance ToHttpApiData FlowType where
  toUrlPiece f = Text.pack (show f)

instance FromHttpApiData FlowType where
  parseUrlPiece "Learning" = pure Learning
  parseUrlPiece "Experimenting" = pure Experimenting
  parseUrlPiece "Troubleshooting" = pure Troubleshooting
  parseUrlPiece "Flowing" = pure Flowing
  parseUrlPiece "Rework" = pure Rework
  parseUrlPiece "Note" = pure Note
  parseUrlPiece _txt = pure Other

data FlowState
  = FlowState
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String
      }
  | FlowNote
      { _flowUser :: String,
        _flowStart :: UTCTime,
        _flowDir :: String,
        _flowNote :: Text.Text
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Flow = Flow
  { _flowType :: FlowType,
    _flowState :: FlowState
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
