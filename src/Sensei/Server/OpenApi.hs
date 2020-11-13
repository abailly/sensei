{-# LANGUAGE OverloadedStrings #-}
-- | Swagger-based documentation for `booking` API
module Sensei.Server.OpenApi (senseiSwagger) where

import Control.Lens
import Data.Swagger
import System.Exit
import Sensei.API
import Servant.Swagger
import Data.Time

-- instance ToSchema Output

-- instance ToSchema PetType

-- instance ToSchema PetStoreError

-- instance ToSchema User

-- instance ToSchema Payment

-- Orphan instances
instance ToSchema ExitCode where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy

instance ToSchema TimeZone where
  declareNamedSchema _ = return $ NamedSchema (Just "TimeZone") $ mempty & type_ .~ Just SwaggerString

instance ToSchema FlowState
instance ToSchema FlowType
instance ToSchema FlowView
instance ToSchema UserProfile

instance ToParamSchema FlowType
instance ToParamSchema Group
instance ToSchema Trace
instance ToSchema Group
instance ToSchema a => ToSchema (GroupViews a) where
  declareNamedSchema proxy =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions proxy

-- instance ToSchema Pet where
--   declareNamedSchema proxy =
--     genericDeclareNamedSchema defaultSchemaOptions proxy
--       & mapped . schema . description ?~ "A Pet for sale in the Store"
--       & mapped . schema . example
--         ?~ toJSON (Pet "Fifi" Dog 100)

senseiSwagger :: Swagger
senseiSwagger =
  toSwagger senseiAPI
    & info . title .~ "Sensei API"
    & info . version .~ "1.0"
    & info . description ?~ "An API for storing and querying data about one's coding habits and patterns"
    & info . license ?~ ("All Rights Reserved")
