module Sensei.Backend.Class where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Sensei.Event (Event)

class (ToJSON backend, FromJSON backend, Show backend) => IsBackend backend where
  postEvent :: Monad m => backend -> Event -> m ()
