{-# LANGUAGE TemplateHaskell #-}

module Sensei.Article
  ( Article (..),
    articleUser,
    articleTimestamp,
    articleDir,
    article,
    articleRkey,
    articleDate,
    parseArticleJSON,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- | Article operation event
data Article
  = PublishArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _article :: Text,
        _articleDate :: Maybe UTCTime
      }
  | UpdateArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _articleRkey :: Text, -- TID/rkey portion only
        _article :: Text,
        _articleDate :: Maybe UTCTime
      }
  | DeleteArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _articleRkey :: Text -- TID/rkey portion only
      }
  deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON (PublishArticle user ts dir art date) =
    object
      [ "operation" .= ("PublishArticle" :: Text),
        "articleUser" .= user,
        "articleTimestamp" .= ts,
        "articleDir" .= dir,
        "article" .= art,
        "articleDate" .= date
      ]
  toJSON (UpdateArticle user ts dir rkey art date) =
    object
      [ "operation" .= ("UpdateArticle" :: Text),
        "articleUser" .= user,
        "articleTimestamp" .= ts,
        "articleDir" .= dir,
        "articleRkey" .= rkey,
        "article" .= art,
        "articleDate" .= date
      ]
  toJSON (DeleteArticle user ts dir rkey) =
    object
      [ "operation" .= ("DeleteArticle" :: Text),
        "articleUser" .= user,
        "articleTimestamp" .= ts,
        "articleDir" .= dir,
        "articleRkey" .= rkey
      ]

instance FromJSON Article where
  parseJSON = withObject "Article" $ \obj -> do
    operation <- obj .: "operation"
    case operation of
      "PublishArticle" ->
        PublishArticle
          <$> obj .: "articleUser"
          <*> obj .: "articleTimestamp"
          <*> obj .: "articleDir"
          <*> obj .: "article"
          <*> obj .:? "articleDate"
      "UpdateArticle" ->
        UpdateArticle
          <$> obj .: "articleUser"
          <*> obj .: "articleTimestamp"
          <*> obj .: "articleDir"
          <*> obj .: "articleRkey"
          <*> obj .: "article"
          <*> obj .:? "articleDate"
      "DeleteArticle" ->
        DeleteArticle
          <$> obj .: "articleUser"
          <*> obj .: "articleTimestamp"
          <*> obj .: "articleDir"
          <*> obj .: "articleRkey"
      _ -> fail $ "Unknown Article operation: " <> operation

parseArticleJSON :: Natural -> Value -> Parser Article
parseArticleJSON v = withObject "Article" $ \obj -> do
  case v of
    12 ->
      -- we only have publish in version 12
      PublishArticle
        <$> obj .: "articleUser"
        <*> obj .: "articleTimestamp"
        <*> obj .: "articleDir"
        <*> obj .: "article"
        <*> pure Nothing -- No articleDate in version 12
    _ -> parseJSON (Object obj)

makeLenses ''Article
