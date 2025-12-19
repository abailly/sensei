{-# LANGUAGE TemplateHaskell #-}

module Sensei.Article (
    Article (..),
    articleUser,
    articleTimestamp,
    articleDir,
    article
) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Article operation event
data Article = PublishArticle
    { _articleUser :: Text
    , _articleTimestamp :: UTCTime
    , _articleDir :: Text
    , _article :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON Article where
    toJSON (PublishArticle user ts dir art) =
        object
            [ "articleUser" .= user
            , "articleTimestamp" .= ts
            , "articleDir" .= dir
            , "article" .= art
            ]

instance FromJSON Article where
    parseJSON = withObject "Article" $ \obj ->
        PublishArticle
            <$> obj .: "articleUser"
            <*> obj .: "articleTimestamp"
            <*> obj .: "articleDir"
            <*> obj .: "article"

makeLenses ''Article
