{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sensei.FlowType where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), Value (String), withText)
import Data.Aeson.Types (FromJSONKeyFunction (FromJSONKeyTextParser), toJSONKeyText)
import Data.Text (Text)
import Data.Text.ToText (ToText (..))
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data FlowType
  = FlowType Text
  | Note
  | End
  | Other
  deriving (Eq, Show, Ord)

makeLenses ''FlowType

instance ToJSONKey FlowType where
  toJSONKey = toJSONKeyText toUrlPiece

instance FromJSONKey FlowType where
  fromJSONKey = FromJSONKeyTextParser parseFlowType

instance ToJSON FlowType where
  toJSON (FlowType f) = String f
  toJSON Note = "Note"
  toJSON End = "End"
  toJSON Other = "Other"

instance ToText FlowType where
  toText (FlowType f) = f
  toText Note = "Note"
  toText End = "End"
  toText Other = "Other"

parseFlowType :: Applicative f => Text -> f FlowType
parseFlowType t =
  case t of
    "Note" -> pure Note
    "End" -> pure End
    "Other" -> pure Other
    other -> pure $ FlowType other

instance FromJSON FlowType where
  parseJSON = withText "FlowType" $ parseFlowType

instance ToHttpApiData FlowType where
  toUrlPiece (FlowType f) = f
  toUrlPiece Note = "Note"
  toUrlPiece End = "End"
  toUrlPiece Other = "Other"

instance FromHttpApiData FlowType where
  parseUrlPiece = parseFlowType

-- | Default flow types when user does not define her own list
-- These are the flow types available for recording, on top of the
-- standard ones which are `Note`, `End` and `Other`
defaultFlowTypes :: [FlowType]
defaultFlowTypes =
  FlowType
    <$> [ "Experimenting",
          "Troubleshooting",
          "Flowing",
          "Rework",
          "Meeting",
          "Learning"
        ]
