{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRoutes where

import Data.LanguageCodes
import Data.LanguageCodes.Arbitrary ()

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time.Calendar

import Routes

import Test.Framework
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary PageFormat where
    arbitrary = genericArbitrary

arbitraryName :: Gen Text
arbitraryName = Text.pack <$> listOf (elements ['a'..'z'])

instance Arbitrary Sitemap where
    arbitrary = oneof [ pure Index
                      , ArticleView <$> arbitrary <*> arbitraryName
                      , MetaView <$> arbitraryName <*> arbitrary
                      , Feed <$> arbitrary
                      , pure SiteScript
                      , pure PrintStylesheet
                      , pure CodeStylesheet
                      ]

test_index_URL = do
    assertEqual "" (routeURL Index)
    assertEqual (Just Index) $ parseURL "/"

test_article_URL = do
    let testArticle = ArticleView (fromGregorian 2020 01 01) "test"
    assertEqual  "/2020-01-01/test" (routeURL testArticle)
    assertEqual (Just testArticle) (parseURL "/2020/01/01/test")
    assertEqual (Just testArticle) (parseURL "/2020-01-01/test/")

test_meta_URL = do
    assertEqual "/meta" (routeURL $ MetaView "meta" Nothing)
    assertEqual (Just $ MetaView "meta" Nothing) (parseURL "/meta/")
    assertEqual "/meta.pdf" (routeURL $ MetaView "meta" (Just Pdf))

test_feed_URL = do
    assertEqual "/feed/en" (routeURL $ Feed EN)

prop_routeURL_parseURL route =
    let url = routeURL route
    in parseURL url == Just route
