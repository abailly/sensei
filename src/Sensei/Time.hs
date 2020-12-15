{-# LANGUAGE OverloadedStrings #-}

module Sensei.Time (Timestamp (..), module Data.Time) where

import Data.Aeson
import Data.Time

newtype Timestamp = Timestamp {timestamp :: UTCTime}
  deriving (Eq, Show)

instance ToJSON Timestamp where
  toJSON (Timestamp ts) = object ["timestamp" .= ts]

instance FromJSON Timestamp where
  parseJSON = withObject "Timestamp" $ \o -> Timestamp <$> o .: "timestamp"
