{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Sensei.Server.Options where

import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import Sensei.API
import Sensei.Version

data Options
  = ClientOptions [String]
  | ServerOptions [String]
  deriving (Show, Eq)

runOptionsParser ::
  [String] -> Either Text Options
runOptionsParser arguments =
  case execParserPure defaultPrefs optionsParserInfo arguments of
    Success opts -> Right opts
    Failure f -> Left (Text.pack . show . fst3 $ execFailure f "")
    _ -> Left "Unexpected completion invoked"
  where
    fst3 (a, _, _) = a

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (commandsParser <**> helper)
    ( progDesc $
        unlines
          [ "sensei - Virtual assistant for developers",
            "version: " <> showVersion senseiVersion <> ", storage: " <> show currentVersion
          ]
    )

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command "server" (info serverOptions (progDesc "Run sensei server. Pass server's arguments after a '--'."))
        <> command "client" (info clientOptions (progDesc "Run sensei client. Pass client's arguments after a '--'."))
    )

clientOptions :: Parser Options
clientOptions =
  ClientOptions <$> many (strArgument (help "any argument"))

serverOptions :: Parser Options
serverOptions =
  ServerOptions <$> many (strArgument (help "any argument"))

parseSenseiOptions ::
  IO Options
parseSenseiOptions = execParser optionsParserInfo
