{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sensei.Server.Auth.API where

import Data.Text (Text)
import Sensei.Server.Auth.Types
import Servant as S
import qualified Servant.Auth as SA

-- * Types

type LoginAPI =
  Summary "Allows users to login passing in credentials."
    :> Description "If successful, this will set cookies containing user's data in the form of JWT token."
    :> "signin"
    :> ReqBody '[JSON] Credentials
    :> Post
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             NoContent
         )

type RegisterAPI =
  Summary
    "User registration endpoint."
    :> Description
         "Registration is successful iff. the user \
         \ provides a valid signed token, which is provided by another user \
         \ (see the @/tokens@ endpoint)."
    :> "signup"
    :> ReqBody '[JSON] UserRegistration
    :> Post '[JSON] NoContent

type KeysAPI =
  Summary "Provide this server's public key(s), for external validation of tokens"
    :> Description
         "Services relying on this authentication server can use those keys to validate provided tokens, \
         \ both authentication tokens and registration tokens. Of course, this assumes the service can trust \
         \ the key(s) indeed come from this authentication server hence should always be done in a trusted \
         \ settings, either using HTTPS with certificate authentication or as part of a controlled deployment \
         \ stack."
    :> "keys"
    :> Get '[JSON] [JWK]

type TokensAPI =
  Summary
    "Registration tokens creation endpoint."
    :> Description
         "An already authenticated user can retrieve tokens to share with \
         \ other users and let them register with this app."
    :> "tokens"
    :> Get '[OctetStream] SerializedToken

type AuthAPI =
  Summary
    "A endpoint to validate authenticated access."
    :> Description
         "This is expected to be used by a reverse proxy which can query that endpoint, \
         \ passing an arbitrary target path. This server will verify the passed credentials and potentially return a www-authenticate \
         \ header to request authentication."
    :> "auth"
    :> CaptureAll "path" Text
    :> Get '[JSON] (Headers '[Header "www-authenticate" String] NoContent)

-- | Protected endpoints require a valid 'AuthenticationToken' which can be provided
-- through a 'JWT' token, or a 'Cookie'
type Protected = Auth '[SA.JWT, SA.Cookie] AuthenticationToken

-- | Authentication endpoint
-- this provides 2 authentication schemes for users:
--
--  * `JWT`: Expects an @Authorization: Bearer XXXX@ header in the query with claims
--    of the shape of `AuthenticationToken`. This should be provided by an external auth
--    provider
--  * `BasicAuth`: Expects an @Authorization: Basic XXXX@ header in the query where @XXX@
--    is a hashed login:password pair. This is useful only in testing and staging context.
type AuthAPIServer =
  LoginAPI
    :<|> RegisterAPI
    :<|> KeysAPI
    :<|> Protected :> TokensAPI
    :<|> Protected :> Header "x-original-method" Text :> AuthAPI
