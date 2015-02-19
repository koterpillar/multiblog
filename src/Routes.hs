{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Routes where

import Prelude hiding ((.))

import Control.Category (Category ((.)))

import Data.Monoid
import qualified Data.Text as T
import Data.Time

import Text.Boomerang.TH (makeBoomerangs)

import Web.Routes.Boomerang

import Language

data Sitemap = Index
           | Yearly Integer
           | Monthly Integer Int
           | Daily Day
           | ArticleView Day String
           | MetaView String
           | Feed Language
           | SiteScript
           deriving (Eq, Show)

makeBoomerangs ''Sitemap

rDay :: Boomerang TextsError [T.Text] r (Day :- r)
rDay = xpure mkDay parseDay . (integer </> int </> int)
    where mkDay (y :- m :- d :- x) = fromGregorian y m d :- x
          parseDay (day :- x) = let (y, m, d) = toGregorian day in Just $ y :- m :- d :- x

-- TODO: This will error on strings which are not language codes
rLanguage :: Boomerang TextsError [T.Text] r (Language :- r)
rLanguage = xpure mkLang parseLang . anyString
    where mkLang (str :- x) = let Just lang = parseLanguage str in lang :- x
          parseLang (lang :- x) = Just $ showLanguage lang :- x

rString :: Boomerang e tok i (T.Text :- o) -> Boomerang e tok i (String :- o)
rString = xmaph T.unpack (Just . T.pack)

anyString :: Boomerang TextsError [T.Text] o (String :- o)
anyString = rString anyText

sitemap :: Boomerang TextsError [T.Text] r (Sitemap :- r)
sitemap = mconcat
    [ rIndex
    , rYearly . integer
    , rMonthly . integer </> int
    , rDaily . rDay
    , rFeed . "feed" </> rLanguage
    , rSiteScript . "site.js"
    , rArticleView . rDay </> anyString
    , rMetaView . anyString
    ]
