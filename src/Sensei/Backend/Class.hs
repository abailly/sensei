{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Sensei.Backend.Class where

import Data.Dynamic (Dynamic (..), fromDynamic, toDyn)
import qualified Data.Map as Map
import Data.Typeable (Proxy (Proxy), Typeable)
import Sensei.Event (Event)
import Type.Reflection (SomeTypeRep, someTypeRep)

-- | Handles a single `Event` for the given `backend`.
newtype BackendHandler backend m = BackendHandler {handleEvent :: backend -> Event -> m ()}

-- | Provides type-indexed mapping to `BackendHandler` instances.
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
  Maybe (BackendHandler backend m)
lookup proxy Backends{backendsMap} =
  Map.lookup (someTypeRep proxy) backendsMap >>= fromDynamic

-- | Insert an handler running in some `Monad` m with the `Backends` map.
--
-- NOTE: In practice, `m` must be `IO` otherwise lookup will fail at runtime. This is so
-- because handlers are created within the `IO` monad early in the bootstrap process.
insert ::
  forall m backend.
  (Typeable m, Typeable backend) =>
  BackendHandler backend m ->
  Backends ->
  Backends
insert backend Backends{backendsMap} =
  Backends{backendsMap = Map.insert (someTypeRep (Proxy @backend)) (toDyn backend) backendsMap}
