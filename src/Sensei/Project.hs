{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Handles classification of flows by projects
module Sensei.Project where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import Data.Text.ToText (ToText, toText)
import System.FilePath (takeBaseName, takeDirectory)
import Text.Regex.TDFA ((=~))

newtype ProjectName = ProjectName {projectName :: Text}
    deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, IsString)

newtype Regex = Regex {regex :: Text}
    deriving newtype (Eq, Show, Read, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, IsString)

instance ToText ProjectName where
    toText = projectName

type ProjectsMap = Map.Map Regex ProjectName

selectProject :: Map.Map Regex ProjectName -> Text -> ProjectName
selectProject projectsMap directory =
    fromMaybe
        (ProjectName $ pack $ findBaseName $ unpack directory)
        (Map.foldrWithKey (findMatch directory) Nothing projectsMap)
  where
    findBaseName path =
        case takeBaseName path of
            "" -> takeBaseName (takeDirectory path)
            other -> other
    findMatch :: Text -> Regex -> ProjectName -> Maybe ProjectName -> Maybe ProjectName
    findMatch source (Regex re) project Nothing =
        if source =~ re
            then Just project
            else Nothing
    findMatch _ _ _ result = result
