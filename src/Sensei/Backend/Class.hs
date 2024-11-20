{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensei.Backend.Class where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Sensei.Client.Monad (ClientMonad, Config)
import Sensei.Event (Event)
import Type.Reflection (SomeTypeRep, someTypeRep)

data BackendIO backend m = BackendIO
  { send :: forall a. Config backend -> ClientMonad (Config backend) a -> m a
  }

class (ToJSON backend, FromJSON backend, Show backend, Typeable backend) => IsBackend backend where
  postEvent :: Monad m => backend -> BackendIO backend m -> Event -> m ()

-- | Provides type-indexed mapping to `BackendIO` instances.
newtype Backends (m :: Type -> Type) = Backends {backendsMap :: Map.Map SomeTypeRep Dynamic}

lookup :: forall m proxy backend. (Typeable m, Typeable backend) => proxy backend -> Backends m -> Maybe (BackendIO backend m)
lookup proxy Backends{backendsMap} =
  Map.lookup (someTypeRep proxy) backendsMap >>= fromDynamic
