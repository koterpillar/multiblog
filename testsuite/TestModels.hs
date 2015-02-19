{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module TestModels where

import Data.DeriveTH
import qualified Data.Map as M
import qualified Data.Set as S

import Models

import Test.Framework

import Arbitrary

derive makeArbitrary ''Article
derive makeArbitrary ''Meta

derive makeArbitrary ''AppState


fall = flip all

prop_allLanguages_hasEveryArticle app =
    fall (appArticles app) $ \article ->
        fall (M.keys (arContent article)) $ \lang ->
            S.member lang $ allLanguages app

prop_allLanguages_hasEveryMeta app =
    fall (appMeta app) $ \meta ->
        fall (M.keys (mtContent meta)) $ \lang ->
            S.member lang $ allLanguages app
