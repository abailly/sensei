{-# LANGUAGE OverloadedStrings #-}

-- | Swagger-based documentation for `booking` API
module Sensei.Server.OpenApi (senseiSwagger) where

import Control.Lens
import Data.Swagger hiding (Reference)
import Data.Text (pack)
import Data.Time
import Sensei.API
import Sensei.Version
import Servant.Swagger
import System.Exit

-- Orphan instances
-- TODO: provide better/more specific return types in the API
instance ToSchema ExitCode where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy

instance ToSchema TimeZone where
  declareNamedSchema _ = return $ NamedSchema (Just "TimeZone") $ mempty & type_ .~ Just SwaggerString

instance ToSchema FlowType where
  declareNamedSchema _ = return $ NamedSchema (Just "FlowType") $ mempty & type_ .~ Just SwaggerString

instance ToSchema Color where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Color") $
        mempty
          & description .~ Just "An RGB color represented as an hexadecimal string"
          & type_ .~ Just SwaggerString

instance ToSchema TimeDifference where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "TimeDifference") $
        mempty
          & description .~ Just "A time difference, positive or negative, expressed as a number of seconds"
          & type_ .~ Just SwaggerNumber

instance ToParamSchema FlowType where
  toParamSchema _ =
    mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ ["End", "Note", "Other", "<any string>"]

instance ToSchema FlowState

instance ToSchema FlowView

instance ToSchema NoteView

instance ToSchema CommandView

instance ToSchema UserProfile

instance ToSchema FlowSummary

instance ToParamSchema Group

instance ToSchema Trace
instance ToSchema Flow
instance ToSchema Group

instance ToParamSchema Reference where
  toParamSchema _ =
        mempty
          & enum_ ?~ ["latest", "head", "<any natural number>"]
          & type_ .~ Just SwaggerString

instance ToSchema a => ToSchema (GroupViews a) where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy

instance ToSchema Versions

instance ToSchema Event where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Event") $
        mempty
          & description ?~ "A generic type of events encapsulating both Flows and Traces"
          & type_ ?~ SwaggerObject

senseiSwagger :: Swagger
senseiSwagger =
  toSwagger senseiAPI
    & info . title .~ "Sensei API"
    & info . version .~ pack (showVersion senseiVersion)
    & info . description ?~ "An API for storing and querying data about one's coding habits and patterns"
    & info . license ?~ ("All Rights Reserved")
