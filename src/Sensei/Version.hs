{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Types and functions to expose and manipulate the server's version
module Sensei.Version
  ( CheckVersion, checkVersion,
    senseiVersion,
    Version,
  )
where

import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Version
import Network.Wai
import Paths_sensei (version)
import Servant
import Servant.Server.Internal (Delayed (..))
import Servant.Server.Internal.DelayedIO

-- | The current Sensei's version
--
-- This is the code representation of the version string definied in the `sensei.cabal`
-- file.
senseiVersion :: Version
senseiVersion = version

checkVersion :: Version -> Version -> Either T.Text ()
checkVersion expected actual =
  if haveSameMajorMinor expected actual
  then pure ()
  else Left ("Incorrect X-API-Version, found " <> T.pack (showVersion actual) <> ", expected " <> T.pack (showVersion expected))

haveSameMajorMinor :: Version -> Version -> Bool
haveSameMajorMinor expected actual = take 2 (versionBranch expected) == take 2 (versionBranch actual)

-- | A type-level "combinator" to mark part of an API as requiring a version check
--
-- @@
-- type MyApi = "foo" :> Capture "bar" Text :> Get [JSON] Bar
--     :<|> CheckVersion :> "baz" :> ReqBody [JSON] Baz :> Post [JSON] NoContent
-- @@
data CheckVersion :: *

instance
  (HasServer api context) =>
  HasServer (CheckVersion :> api) context
  where

  -- CheckVersion is a "marker" type so it does not modify the structure
  -- of the underlying sub-`api`
  type ServerT (CheckVersion :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addCheck subserver (withRequest headerCheck)
    where

      -- adds a `headersD` check to the current set of `Delayed` checks
      -- see https://hackage.haskell.org/package/servant-server-0.16/docs/Servant-Server-Internal-Delayed.html
      addCheck :: Delayed env a -> DelayedIO () -> Delayed env a
      addCheck Delayed {..} new =
        Delayed
          { headersD = new >> headersD,
            -- `DelayedIO` is a Monad so it's perfectly fine to sequence checks
            ..
          }

      headerCheck :: Request -> DelayedIO ()
      headerCheck req =
        either errReq pure mev
        where
          mev :: Either T.Text ()
          mev =
            maybe (Left "Cannot find header X-API-Version") Right (lookup "x-api-version" (requestHeaders req))
              >>= parseHeader
              >>= checkVersion senseiVersion

          errReq :: T.Text -> DelayedIO ()
          errReq txt = delayedFailFatal $ err406 {errBody = encodeUtf8 (fromStrict txt)}
