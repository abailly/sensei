{-# LANGUAGE OverloadedStrings #-}

module Network.CORS
  ( WithCORS (..),
    handleCors,
    rejectInvalidHost,
  )
where

import Network.HTTP.Types.Status (status400)
import Network.Wai (Middleware, requestHeaderHost, responseLBS)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    Origin,
    cors,
    simpleHeaders,
  )

data WithCORS
  = NoCORS
  | WithCORS [Origin]

-- | Reject request if `Host` header is not equal to given `serverName`
rejectInvalidHost :: Origin -> Middleware
rejectInvalidHost serverName app req resp =
  if requestHeaderHost req == Just serverName
    then app req resp
    else resp $ responseLBS status400 [] ""

-- | Handle CORS accepting only the specific set of `origins`.
-- If given an empty list, produces a `Middleware` that accepts anything.
handleCors :: [Origin] -> Middleware
handleCors [] = policy Nothing
handleCors origins = policy $ Just (origins, True)

policy :: Maybe ([Origin], Bool) -> Middleware
policy origins =
  cors $
    const $
      Just $
        CorsResourcePolicy
          { corsOrigins = origins,
            corsMethods = ["OPTIONS", "GET", "POST", "PUT", "HEAD"],
            corsRequestHeaders = "content-type" : "authorization" : simpleHeaders,
            corsExposedHeaders = Nothing,
            corsMaxAge = Nothing,
            corsVaryOrigin = False,
            corsRequireOrigin = False,
            corsIgnoreFailures = False
          }
