{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Server.Tags where

import Control.Lens
import Data.HashSet.InsOrd
import Data.Swagger
import Data.Text
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant hiding (Context)
import Servant.Client.Core
import Servant.Swagger
import Servant.Swagger.Internal

infixl 8 :?

-- | Annotate an 'api' fragment with some 'doc' string.
data (:?) api (doc :: Symbol)
  deriving (Typeable)

instance (HasServer (api :> api') ctx) => HasServer (api :? doc :> api') ctx where
  type ServerT (api :? doc :> api') m = ServerT (api :> api') m
  route _ = route (Proxy :: Proxy (api :> api'))
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy (api :> api'))

instance (RunClient m, HasClient m (api :> api')) => HasClient m (api :? doc :> api') where
  type Client m (api :? doc :> api') = Client m (api :> api')
  clientWithRoute pm _ r = clientWithRoute pm (Proxy :: Proxy (api :> api')) r
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy (api :> api')) nt cl

instance (KnownSymbol doc, KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (Capture' mods sym a :? doc :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & addParam param
      & prependPath capture
      & addDefaultResponse404 tname
    where
      pname = symbolVal (Proxy :: Proxy sym)
      tname = pack pname
      capture = "{" <> pname <> "}"
      param =
        mempty
          & name .~ tname
          & description ?~ pack (symbolVal (Proxy :: Proxy doc))
          & required ?~ True
          & schema
            .~ ParamOther
              ( mempty
                  & in_ .~ ParamPath
                  & paramSchema .~ toParamSchema (Proxy :: Proxy a)
              )

instance (KnownSymbol doc, KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (QueryParam' mods sym a :? doc :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & addParam param
      & addDefaultResponse400 tname
    where
      pname = symbolVal (Proxy :: Proxy sym)
      tname = pack pname
      param =
        mempty
          & name .~ tname
          & description ?~ pack (symbolVal (Proxy :: Proxy doc))
          & required ?~ False
          & schema .~ ParamOther sch
      sch =
        mempty
          & in_ .~ ParamQuery
          & paramSchema .~ toParamSchema (Proxy :: Proxy a)

instance (KnownSymbol doc, KnownSymbol sym, ToParamSchema a, HasSwagger sub) => HasSwagger (QueryParams sym a :? doc :> sub) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy sub)
      & addParam param
      & addDefaultResponse400 tname
    where
      tname = pack (symbolVal (Proxy :: Proxy sym))
      param =
        mempty
          & name .~ tname
          & description ?~ pack (symbolVal (Proxy :: Proxy doc))
          & schema .~ ParamOther sch
      sch =
        mempty
          & in_ .~ ParamQuery
          & paramSchema .~ pschema
      pschema =
        mempty
          & type_ ?~ SwaggerArray
          & items ?~ SwaggerItemsPrimitive (Just CollectionMulti) (toParamSchema (Proxy :: Proxy a))

data Tags (sym :: Symbol)
  deriving (Typeable)

instance HasServer api ctx => HasServer (Tags tags :> api) ctx where
  type ServerT (Tags tags :> api) m = ServerT api m
  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

instance (KnownSymbol tags, HasSwagger api) => HasSwagger (Tags tags :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & allOperations . tags %~ union (fromList [pack (symbolVal (Proxy :: Proxy tags))])

instance (RunClient m, HasClient m api) => HasClient m (Tags t :> api) where
  type Client m (Tags t :> api) = Client m api
  clientWithRoute pm _ r = clientWithRoute pm (Proxy :: Proxy api) r
  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt cl
