{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Sensei.Article (
    ArticleOp (..),
    ArticleOperation (..),
    articleOperation,
    articleUser,
    articleTimestamp,
    articleDir,
) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Operation types for articles
data ArticleOperation
    = Publish
    deriving (Eq, Show, Generic)

instance ToJSON ArticleOperation where
    toJSON Publish = "Publish"

instance FromJSON ArticleOperation where
    parseJSON = withText "ArticleOperation" $ \case
        "Publish" -> pure Publish
        other -> fail $ "Unknown ArticleOperation: " <> show other

-- | Article operation event
data ArticleOp = ArticleOp
    { _articleOperation :: ArticleOperation
    , _articleUser :: Text
    , _articleTimestamp :: UTCTime
    , _articleDir :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON ArticleOp where
    toJSON (ArticleOp op user ts dir) =
        object
            [ "articleOperation" .= op
            , "articleUser" .= user
            , "articleTimestamp" .= ts
            , "articleDir" .= dir
            ]

instance FromJSON ArticleOp where
    parseJSON = withObject "ArticleOp" $ \obj ->
        ArticleOp
            <$> obj .: "articleOperation"
            <*> obj .: "articleUser"
            <*> obj .: "articleTimestamp"
            <*> obj .: "articleDir"

makeLenses ''ArticleOp
