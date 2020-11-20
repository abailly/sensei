{-# LANGUAGE OverloadedStrings #-}
-- | Swagger-based documentation for `booking` API
module Sensei.Server.OpenApi (senseiSwagger) where

import Control.Lens
import Data.Swagger
import System.Exit
import Sensei.API
import Servant.Swagger
import Data.Time

-- Orphan instances
-- TODO: provide better/more specific return types in the API
instance ToSchema ExitCode where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy

instance ToSchema TimeZone where
  declareNamedSchema _ = return $ NamedSchema (Just "TimeZone") $ mempty & type_ .~ Just SwaggerString

instance ToSchema FlowType where
  declareNamedSchema _ = return $ NamedSchema (Just "FlowType") $ mempty & type_ .~ Just SwaggerString

instance ToParamSchema FlowType where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ [ "End", "Note", "Other", "<any string>" ]


instance ToSchema FlowState
instance ToSchema FlowView
instance ToSchema NoteView
instance ToSchema CommandView
instance ToSchema UserProfile

instance ToParamSchema Group
instance ToSchema Trace
instance ToSchema Group
instance ToSchema a => ToSchema (GroupViews a) where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy


senseiSwagger :: Swagger
senseiSwagger =
  toSwagger senseiAPI
    & info . title .~ "Sensei API"
    & info . version .~ "1.0"
    & info . description ?~ "An API for storing and querying data about one's coding habits and patterns"
    & info . license ?~ ("All Rights Reserved")
