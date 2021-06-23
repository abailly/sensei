{-# LANGUAGE OverloadedStrings #-}

module Sensei.Server.LinksSpec where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (isInfixOf, pack)
import Data.Time.Calendar as Date
import Sensei.Group (Group (..))
import Sensei.Server.Links (LinkParam (..), href, linkParams, periodLinks, uriToText)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, arbitrary, counterexample, forAll)

spec :: Spec
spec = describe "Links Headers" $ do
  prop "generates prev/next links for periods" prop_generatePrevNextLinksForPeriods

prop_generatePrevNextLinksForPeriods :: Property
prop_generatePrevNextLinksForPeriods =
  forAll days $ \day ->
    let groups = [Week, Month, Quarter, Year]
        links = concat $ mapMaybe (periodLinks "bob" day day) groups
        linkUris = uriToText . href <$> links
        params = concat $ map fst . linkParams <$> links
        inUri t = any (t `isInfixOf`) linkUris
     in counterexample (show links) $
          nub links == links
            && all inUri (pack . show <$> groups)
            && Other "from" `elem` params
            && Other "to" `elem` params
            && Other "period" `elem` params

days :: Gen Day
days = toEnum <$> arbitrary
