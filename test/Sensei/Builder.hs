
-- | Specific help functions and types to help build and manipulate
--   Sensei's types and API
module Sensei.Builder (
  postEvent,
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
  aBskyBackend,
  postNote,
) where

import Control.Lens ((%~))
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Time.Lens (Lens, modL, month, seconds)
import Network.URI.Extra (uriFromString)
import Preface.Codec
import Sensei.API
import Sensei.Backend (Backend (..))
import Sensei.Bsky.Core (BskyBackend (..), BskyLogin (..))
import Sensei.TestHelper

postEvent :: [Event] -> WaiSession (Maybe (Encoded Hex)) SResponse
postEvent = postJSON "/api/log/arnaud"

postEvent_ :: [Event] -> WaiSession (Maybe (Encoded Hex)) ()
postEvent_ = void . postEvent

postFlow :: Flow -> WaiSession (Maybe (Encoded Hex)) SResponse
postFlow = postEvent . pure . EventFlow

postFlow_ :: Flow -> WaiSession (Maybe (Encoded Hex)) ()
postFlow_ = postEvent_ . pure . EventFlow

postNote_ :: NoteFlow -> WaiSession (Maybe (Encoded Hex)) ()
postNote_ = postEvent_ . pure . EventNote

postNote :: NoteFlow -> WaiSession (Maybe (Encoded Hex)) SResponse
postNote = postEvent . pure . EventNote

postTrace_ :: Trace -> WaiSession (Maybe (Encoded Hex)) ()
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

aBskyBackend :: Backend
aBskyBackend =
  Backend $
    BskyBackend
      { login =
          BskyLogin
            { identifier = "bob.bsky.social"
            , password = "password"
            }
      , pdsUrl = fromJust $ uriFromString "https://some.social"
      }
