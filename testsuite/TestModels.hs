{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TestModels where

import Data.DeriveTH
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Web.Twitter.Conduit as TW

import Models

import Test.Framework
import Test.QuickCheck.Instances ()

import Arbitrary ()

derive makeArbitrary ''Article

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

instance Arbitrary TW.Credential where
    arbitrary = do
        token <- arbitrary
        secret <- arbitrary
        return $
            TW.Credential
                [("oauth_token", token), ("oauth_token_secret", secret)]

derive makeArbitrary ''ServiceAuth

derive makeArbitrary ''CrossPost

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
