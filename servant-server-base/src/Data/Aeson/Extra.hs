{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aeson.Extra (
    module Data.Aeson,
    toData,
    parseJsonError,
    (!!),
    (.!>>=),
    (.:=>),
) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types hiding (Success)
import qualified Data.HashMap.Strict as H
import Data.Maybe.Extra
import Data.Monoid
import Data.Text as ST
import GHC.Stack (HasCallStack)
import Prelude hiding ((!!))

-- | An operator similar to (.!=) but provides a monadic default value.
(.!>>=) :: Parser (Maybe a) -> Parser a -> Parser a
(.!>>=) = flip fromMaybeM

-- | A more verbose version of (!) operator that reports invalid keys and objects
(!!) :: H.HashMap ST.Text Value -> ST.Text -> Value
o !! k =
    if not $ k `H.member` o
        then error $ show k ++ " is not a key in object " ++ show o
        else o H.! k

{- | Safely accesses a JSON object where the value at a key is text. It takes an
 object, a key, and a continuation of what to do when this key is present.

 Example:

 @
     data D = A | B | C
     instance FromJSON D where
       parseJSON = withObject "D" $ \o ->
         o .:=> "tag" $ \case
           "A" -> pure A
           "B" -> pure B
           "C" -> pure C
           _ -> fail "Cannot parse the tag of D"
 @
-}
(.:=>) :: Object -> Text -> (Text -> Parser a) -> Parser a
o .:=> k = \m -> o .: fromText k >>= withText ("Object with mandatory key " <> unpack k) m

-- | Unconditionally convert a JSon Value to some data type
toData :: (FromJSON a, HasCallStack) => Value -> a
toData v = case fromJSON v of
    Success suc -> suc
    Error txt -> error $ "fail to convert JSON value, " ++ txt ++ ": " ++ show v

parseJsonError :: (Show f) => String -> f -> Value -> Parser a
parseJsonError parsing fragment value = fail $ "unrecognized " ++ parsing ++ ": " ++ show fragment ++ " when parsing json value: " ++ show value
