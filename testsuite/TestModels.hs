{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestModels where

import           Data.LanguageCodes.Arbitrary      ()

import qualified Data.Map                          as Map
import qualified Data.Set                          as Set

import           Text.Pandoc                       hiding (Meta)

import           Models
import           Types.Content

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances         ()

instance Arbitrary Pandoc where
    arbitrary = pure $ Pandoc nullMeta []

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

instance Arbitrary AppSettings where
    arbitrary = genericArbitrary

instance Arbitrary AppData where
    arbitrary = genericArbitrary

fall :: [a] -> (a -> Bool) -> Bool
fall = flip all

prop_allLanguages_hasEveryArticle :: AppData -> Bool
prop_allLanguages_hasEveryArticle app =
    fall (appArticles app) $ \article ->
        Map.keysSet (arContent article) `Set.isSubsetOf` allLanguages app

prop_allLanguages_hasEveryMeta :: AppData -> Bool
prop_allLanguages_hasEveryMeta app =
    fall (appMeta app) $ \meta ->
        Map.keysSet (mtContent meta) `Set.isSubsetOf` allLanguages app
