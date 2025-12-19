{-# LANGUAGE TemplateHaskell #-}

module Sensei.Article
  ( Article (..),
    articleUser,
    articleTimestamp,
    articleDir,
    article,
    articleRkey,
    parseArticleJSON,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=), Value (Object))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Data.Aeson.Types (Parser)

-- | Article operation event
data Article
  = PublishArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _article :: Text
      }
  | UpdateArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _articleRkey :: Text, -- TID/rkey portion only
        _article :: Text
      }
  | DeleteArticle
      { _articleUser :: Text,
        _articleTimestamp :: UTCTime,
        _articleDir :: Text,
        _articleRkey :: Text -- TID/rkey portion only
      }
  deriving (Eq, Show, Generic)

instance ToJSON Article where
  toJSON (PublishArticle user ts dir art) =
    object
      [ "operation" .= ("PublishArticle" :: Text),
        "articleUser" .= user,
        "articleTimestamp" .= ts,
        "articleDir" .= dir,
        "article" .= art
      ]
  toJSON (UpdateArticle user ts dir rkey art) =
    object
      [ "operation" .= ("UpdateArticle" :: Text),
        "articleUser" .= user,
        "articleTimestamp" .= ts,
        "articleDir" .= dir,
        "articleRkey" .= rkey,
        "article" .= art
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
      "UpdateArticle" ->
        UpdateArticle
          <$> obj .: "articleUser"
          <*> obj .: "articleTimestamp"
          <*> obj .: "articleDir"
          <*> obj .: "articleRkey"
          <*> obj .: "article"
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
        PublishArticle
          <$> obj .: "articleUser"
          <*> obj .: "articleTimestamp"
          <*> obj .: "articleDir"
          <*> obj .: "article"
      _ -> parseJSON (Object obj)

makeLenses ''Article
