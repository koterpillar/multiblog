{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRoutes where

import Data.LanguageCodes
import Data.LanguageCodes.Arbitrary ()
import Data.Time.Calendar

import Routes

import Test.Framework
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary PageFormat where
    arbitrary = genericArbitrary

instance Arbitrary Sitemap where
    arbitrary = genericArbitrary

test_routeURL_parseURL = do
    assertEqual (routeURL Index) ""
    assertEqual (parseURL "/") $ Just Index
    let testArticle = ArticleView (fromGregorian 2020 01 01) "test"
    assertEqual (routeURL testArticle) "/2020-01-01/test"
    assertEqual (parseURL "/2020/01/01/test") $ Just testArticle
    assertEqual (parseURL "/2020-01-01/test/") $ Just testArticle
    assertEqual (routeURL $ MetaView "meta" Nothing) "/meta"
    assertEqual (parseURL "/meta/") $ Just $ MetaView "meta" Nothing
    assertEqual (routeURL $ MetaView "meta" (Just Pdf)) "/meta.pdf"
    assertEqual (routeURL $ Feed EN) "/feed/en"

prop_routeURL_parseURL route =
    let url = routeURL route
    in parseURL url == Just route
