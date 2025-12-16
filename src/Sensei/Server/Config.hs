
-- | Configuration of sensei server
module Sensei.Server.Config where

import Data.Char

data Env = Dev | Prod
  deriving (Eq, Show)

readEnv ::
  String -> Maybe Env
readEnv s =
  case fmap toLower s of
    "dev" -> pure Dev
    "development" -> pure Dev
    "prod" -> pure Prod
    "production" -> pure Prod
    _ -> Nothing

readPort :: Maybe String -> Int
readPort Nothing = 23456
readPort (Just portString) =
  case reads portString of
    (p, []) : _ -> p
    _ -> error ("invalid environment variable SENSEI_SERVER_PORT " <> portString)
