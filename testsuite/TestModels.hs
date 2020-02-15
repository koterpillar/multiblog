{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestModels where

import           Data.LanguageCodes.Arbitrary      ()

import qualified Data.Map                          as M
import qualified Data.Set                          as S

import           Text.Pandoc.Arbitrary

import qualified Web.Twitter.Conduit               as TW

import           Models
import           Types.Content
import           Types.Services

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

instance Arbitrary TW.OAuth where
    arbitrary = do
        key <- arbitrary
        secret <- arbitrary
        return $
            TW.twitterOAuth
                {TW.oauthConsumerKey = key, TW.oauthConsumerSecret = secret}

instance Arbitrary AppServices where
    arbitrary = genericArbitrary

instance Arbitrary TwitterAuth where
    arbitrary = genericArbitrary

instance Arbitrary AppAuth where
    arbitrary = genericArbitrary

instance Arbitrary CrossPost where
    arbitrary = genericArbitrary

instance Arbitrary AppData where
    arbitrary = genericArbitrary

fall :: [a] -> (a -> Bool) -> Bool
fall = flip all

prop_allLanguages_hasEveryArticle app =
    fall (appArticles app) $ \article ->
        fall (M.keys (arContent article)) $ \lang ->
            S.member lang $ allLanguages app

prop_allLanguages_hasEveryMeta app =
    fall (appMeta app) $ \meta ->
        fall (M.keys (mtContent meta)) $ \lang ->
            S.member lang $ allLanguages app
