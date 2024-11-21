{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Backend.Class where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Dynamic (Dynamic (..), fromDynamic, toDyn)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy), Typeable)
import Sensei.Client.Monad (ClientMonad, Config)
import Sensei.Event (Event)
import Type.Reflection (SomeTypeRep, someTypeRep)

data BackendIO backend m = BackendIO
  { send :: forall a. Config backend -> ClientMonad (Config backend) a -> m a
  }

class (ToJSON backend, FromJSON backend, Show backend, Typeable backend) => IsBackend backend where
  postEvent :: Monad m => backend -> BackendIO backend m -> Event -> m ()

-- | Provides type-indexed mapping to `BackendIO` instances.
newtype Backends = Backends {backendsMap :: Map.Map SomeTypeRep Dynamic}

keys :: Backends -> [SomeTypeRep]
keys Backends{backendsMap} = Map.keys backendsMap

empty :: Backends
empty = Backends mempty

lookup ::
  forall m proxy backend.
  (Typeable m, Typeable backend) =>
  proxy backend ->
  Backends ->
  Maybe (BackendIO backend m)
lookup proxy Backends{backendsMap} =
  Map.lookup (someTypeRep proxy) backendsMap >>= fromDynamic

insert ::
  forall m backend.
  (Typeable m, Typeable backend) =>
  BackendIO backend m ->
  Backends ->
  Backends
insert backend Backends{backendsMap} =
  Backends{backendsMap = Map.insert (someTypeRep (Proxy @backend)) (toDyn backend) backendsMap}
