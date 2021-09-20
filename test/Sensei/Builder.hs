{-# LANGUAGE OverloadedStrings #-}

-- | Specific help functions and types to help build and manipulate
--  Sensei's types and API
module Sensei.Builder
  ( postEvent,
    postEvent_,
    postFlow,
    postFlow_,
    postNote_,
    postTrace_,
    anOtherFlow,
    aDay,
    oneAM,
    sixThirtyPM,
    later,
    -- reexported from time-lens
    seconds,
    month,
  )
where

import Control.Lens ((%~))
import Data.Functor (void)
import Data.Time (Day, TimeOfDay (TimeOfDay), UTCTime (UTCTime))
import Data.Time.Lens (Lens, modL, month, seconds)
import Sensei.API
import Sensei.TestHelper

postEvent :: [Event] -> WaiSession () SResponse
postEvent = postJSON "/api/log"

postEvent_ :: [Event] -> WaiSession () ()
postEvent_ = void . postEvent

postFlow :: Flow -> WaiSession () SResponse
postFlow = postEvent . pure . EventFlow

postFlow_ :: Flow -> WaiSession () ()
postFlow_ = postEvent_ . pure . EventFlow

postNote_ :: NoteFlow -> WaiSession () ()
postNote_ = postEvent_ . pure . EventNote

postTrace_ :: Trace -> WaiSession () ()
postTrace_ = postEvent_ . pure . EventTrace

anOtherFlow :: Flow
anOtherFlow = Flow Other "arnaud" (UTCTime aDay 0) "some/directory"

aDay :: Day
aDay = toEnum 50000

oneAM :: TimeOfDay
oneAM = TimeOfDay 1 0 0

sixThirtyPM :: TimeOfDay
sixThirtyPM = TimeOfDay 18 30 0

later :: Num b => b -> Lens UTCTime b -> Flow -> Flow
later dur unit = flowTimestamp %~ modL unit (+ dur)
