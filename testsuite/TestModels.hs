{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TestModels where

import Data.DeriveTH

import Data.LanguageCodes.Arbitrary ()

import qualified Data.Map as M
import qualified Data.Set as S

import Text.Pandoc.Arbitrary

import qualified Web.Twitter.Conduit as TW

import Models
import Types.Content
import Types.Services

import Test.Framework
import Test.QuickCheck.Instances ()

derive makeArbitrary ''Article

derive makeArbitrary ''Layout

derive makeArbitrary ''Meta

derive makeArbitrary ''Link

derive makeArbitrary ''Analytics

instance Arbitrary TW.OAuth where
    arbitrary = do
        key <- arbitrary
        secret <- arbitrary
        return $
            TW.twitterOAuth
            { TW.oauthConsumerKey = key
            , TW.oauthConsumerSecret = secret
            }

derive makeArbitrary ''AppServices

derive makeArbitrary ''TwitterAuth

derive makeArbitrary ''AppAuth

derive makeArbitrary ''CrossPost

derive makeArbitrary ''SiteAddress

derive makeArbitrary ''AppData

fall :: [a] -> (a -> Bool) -> Bool
fall = flip all

prop_allLanguages_hasEveryArticle app =
    fall (appArticles app) $
    \article ->
         fall (M.keys (arContent article)) $
         \lang -> S.member lang $ allLanguages app

prop_allLanguages_hasEveryMeta app =
    fall (appMeta app) $
    \meta ->
         fall (M.keys (mtContent meta)) $
         \lang -> S.member lang $ allLanguages app
