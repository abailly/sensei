{-# LANGUAGE OverloadedStrings #-}
module Sensei.Server.LinksSpec where

import Sensei.Server.Links (periodLinks, href, uriToText)
import Data.Time.Calendar as Date
import Sensei.Group(Group(..))
import Data.Text(isInfixOf, pack)
import Data.Maybe(mapMaybe)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Gen, counterexample, arbitrary, forAll)
import Data.List(nub)

spec :: Spec
spec = describe "Links Headers" $ do
  prop "generates prev/next links for periods" prop_generatePrevNextLinksForPeriods

prop_generatePrevNextLinksForPeriods :: Property
prop_generatePrevNextLinksForPeriods =
  forAll days $ \ day ->
  let groups = [ Week, Month, Quarter, Year ]
      links = concat $ mapMaybe (periodLinks "bob" day day) groups
      linkUris = uriToText . href <$> links
      inUri t = any (t `isInfixOf`) linkUris
  in counterexample (show links) $
     nub links == links &&
     all inUri (pack . show <$> groups)

days :: Gen Day
days = toEnum <$> arbitrary 
