module Sensei.Duration where

import Control.Monad.Identity (Identity)
import Data.Aeson
import Data.Bifunctor
import Data.Time
import Text.Parsec
import Data.Text.Prettyprint.Doc
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Data.Text.Prettyprint.Doc.Render.String (renderString)

-- | A structured representation of a time difference.
--  A `TimeDifference` is represented as fractional number of
--  seconds on the wire (Eg. in JSON, as a number)
data TimeDifference
  = Minutes Integer
  deriving (Eq, Show)

instance ToJSON TimeDifference where
  toJSON (Minutes m) = toJSON $ m * 60

instance FromJSON TimeDifference where
  parseJSON =
    withScientific "TimeDifference" $
      \n ->
        let mins = n / 60
         in pure (Minutes $ floor mins)

toNominalDiffTime :: TimeDifference -> NominalDiffTime
toNominalDiffTime (Minutes m) = fromInteger $ m * 60

instance Pretty TimeDifference where
  pretty (Minutes m) = pretty m <> pretty "m"

prettyPrint ::
  TimeDifference -> String
prettyPrint = renderString . layoutPretty defaultLayoutOptions . pretty

parse ::
  String -> Either String TimeDifference
parse s =
  first show $ runParser timeDifferenceParser () "" s

timeDifferenceParser :: Parsec String () TimeDifference
timeDifferenceParser = do
  num <- integer <* char 'm'
  pure $ Minutes num

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellDef

integer :: ParsecT String u Identity Integer
integer = P.integer lexer
