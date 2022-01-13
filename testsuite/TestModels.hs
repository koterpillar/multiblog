{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestModels where

import           Data.LanguageCodes.Arbitrary      ()

import qualified Data.Map                          as Map
import qualified Data.Set                          as Set

import           Text.Pandoc.Arbitrary

import           Models
import           Types.Content

import           Test.Framework
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances         ()

instance Arbitrary Article where
    arbitrary = genericArbitrary

instance Arbitrary Layout where
    arbitrary = genericArbitrary

instance Arbitrary Meta where
    arbitrary = genericArbitrary

instance Arbitrary Link where
    arbitrary = genericArbitrary

instance Arbitrary Analytics where
    arbitrary = genericArbitrary

instance Arbitrary AppData where
    arbitrary = genericArbitrary

fall :: [a] -> (a -> Bool) -> Bool
fall = flip all

prop_allLanguages_hasEveryArticle app =
    fall (appArticles app) $ \article ->
        fall (Map.keys (arContent article)) $ \lang ->
            Set.member lang $ allLanguages app

prop_allLanguages_hasEveryMeta app =
    fall (appMeta app) $ \meta ->
        fall (Map.keys (mtContent meta)) $ \lang ->
            Set.member lang $ allLanguages app
