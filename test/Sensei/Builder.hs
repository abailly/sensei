{-# LANGUAGE OverloadedStrings #-}

-- | Specific help functions and types to help build and manipulate
--  Sensei's types and API
module Sensei.Builder where

import Data.Functor (void)
import Data.Time (Day, TimeOfDay (TimeOfDay), UTCTime (UTCTime))
import Sensei.API
import Sensei.TestHelper

postEvent :: Event -> WaiSession () SResponse
postEvent = postJSON "/api/log"

postEvent_ :: Event -> WaiSession () ()
postEvent_ = void . postEvent

postFlow :: Flow -> WaiSession () SResponse
postFlow = postEvent . EventFlow

postFlow_ :: Flow -> WaiSession () ()
postFlow_ = postEvent_ . EventFlow

postNote_ :: NoteFlow -> WaiSession () ()
postNote_ = postEvent_ . EventNote

postTrace_ :: Trace -> WaiSession () ()
postTrace_ = postEvent_ . EventTrace

anOtherFlow :: Flow
anOtherFlow = Flow Other "arnaud" (UTCTime (toEnum 50000) 0) "some/directory"

aDay :: Day
aDay = toEnum 50000

oneAM :: TimeOfDay
oneAM = TimeOfDay 1 0 0

sixThirtyPM :: TimeOfDay
sixThirtyPM = TimeOfDay 18 30 0
