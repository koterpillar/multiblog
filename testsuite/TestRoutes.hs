{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRoutes where

import           Data.LanguageCodes
import           Data.LanguageCodes.Arbitrary      ()

import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           Data.Time.Calendar

import           Routes

import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances         ()

instance Arbitrary PageFormat where
    arbitrary = genericArbitrary

arbitraryName :: Gen Text
arbitraryName = Text.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary Sitemap where
    arbitrary =
        oneof
            [ pure Index
            , ArticleView <$> arbitrary <*> arbitraryName
            , MetaView <$> arbitraryName <*> arbitrary
            , Feed <$> arbitrary
            , pure SiteScript
            , pure PrintStylesheet
            , pure CodeStylesheet
            ]

unit_index_URL :: IO ()
unit_index_URL = do
    assertEqual "" "" (routeURL Index)
    assertEqual "" (Just Index) $ parseURL "/"

unit_article_URL :: IO ()
unit_article_URL = do
    let testArticle = ArticleView (fromGregorian 2020 01 01) "test"
    assertEqual "" "/2020-01-01/test" (routeURL testArticle)
    assertEqual "" (Just testArticle) (parseURL "/2020/01/01/test")
    assertEqual "" (Just testArticle) (parseURL "/2020-01-01/test/")

unit_meta_URL :: IO ()
unit_meta_URL = do
    assertEqual "" "/meta" (routeURL $ MetaView "meta" Nothing)
    assertEqual "" (Just $ MetaView "meta" Nothing) (parseURL "/meta/")
    assertEqual "" "/meta.pdf" (routeURL $ MetaView "meta" (Just Pdf))

unit_feed_URL :: IO ()
unit_feed_URL = assertEqual "" "/feed/en" (routeURL $ Feed EN)

prop_routeURL_parseURL :: Sitemap -> Bool
prop_routeURL_parseURL route =
    let url = routeURL route
     in parseURL url == Just route
