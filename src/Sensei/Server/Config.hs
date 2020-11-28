-- | Configuration of sensei server
module Sensei.Server.Config where

data Env = Dev | Prod
  deriving (Eq, Show)


readEnv ::
  String -> Maybe Env
readEnv s =
  case s of
    "dev" -> pure Dev
    "development" -> pure Dev
    "prod" -> pure Prod
    "production" -> pure Prod
    _ -> Nothing
