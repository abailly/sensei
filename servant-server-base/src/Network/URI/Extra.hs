{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Simple extensions and utility functions for the @URI@ type
--
-- THis module is a drop-in replacement for @Network.URI@ which it reexports. Simply replace
--
--     import Network.URI
--
-- with
--
--     import Network.URI.Extra
module Network.URI.Extra
  ( module Network.URI,
    uriFromString,
    uriToString',
    serverHost,
    extendURI,
  )
where

import Data.Aeson
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Network.HostName
import Network.URI

uriFromString :: (MonadFail m) => String -> m URI
uriFromString v = case parseURIReference v of
  Just u -> return u
  Nothing -> fail $ show v <> " is not a valid URI"

-- | Straightforward @String@ representation of a @URI@
uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

instance IsString URI where
  fromString = fromMaybe (error "invalid URI") . uriFromString

instance ToJSON URI where
  toJSON uri = String $ T.pack $ uriToString id uri ""

instance FromJSON URI where
  parseJSON (String v) = uriFromString (T.unpack v)
  parseJSON v = fail $ show v <> " is not a valid URI"

-- | Returns the host part of given @URI@ or an empty string if no authority is defined
serverHost :: URI -> HostName
serverHost (uriAuthority -> Just URIAuth {..}) = uriRegName
serverHost _ = ""

extendURI :: URI -> String -> Maybe String
extendURI uri extension = uriToString' . (`relativeTo` uri) <$> extensionUri
  where
    extensionUri :: Maybe URI
    extensionUri = uriFromString extension
