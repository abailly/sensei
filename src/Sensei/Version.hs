{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Version (CheckVersion, senseiVersion, Version) where

import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Version
import Network.Wai
import Paths_sensei (version)
import Servant
import Servant.Server.Internal (Delayed (..))
import Servant.Server.Internal.DelayedIO

senseiVersion :: Version
senseiVersion = version

data CheckVersion :: *

instance
  (HasServer api context) =>
  HasServer (CheckVersion :> api) context
  where
  type ServerT (CheckVersion :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addCheck subserver (withRequest headerCheck)
    where
      addCheck :: Delayed env a -> DelayedIO () -> Delayed env a
      addCheck Delayed {..} new =
        Delayed
          { headersD = new >> headersD,
            ..
          }

      headerCheck :: Request -> DelayedIO ()
      headerCheck req =
        either errReq pure mev
        where
          mev :: Either T.Text ()
          mev =
            maybe (Left "Cannot find header X-API-Version") Right (lookup "x-api-version" (requestHeaders req)) >>=
            parseHeader >>=
            \ v -> if v == senseiVersion
                   then pure ()
                   else Left ("Incorrect X-API-Version, found " <> T.pack (show v) <> ", expected " <> T.pack (show senseiVersion))

          errReq :: T.Text -> DelayedIO ()
          errReq txt = delayedFailFatal $ err406 { errBody = encodeUtf8 (fromStrict txt) }
