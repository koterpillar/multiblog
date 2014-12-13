{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Routes where

import Prelude hiding ((.))

import Control.Category (Category((.)))

import Data.Monoid
import qualified Data.Text as T
import Data.Time

import Text.Boomerang.TH (makeBoomerangs)

import Web.Routes.Boomerang

data Sitemap = Index
           | Yearly Integer
           | Monthly Integer Int
           | Daily Day
           | ArticleView Day String
           deriving (Eq, Show)

makeBoomerangs ''Sitemap

rDay :: Boomerang TextsError [T.Text] r (Day :- r)
rDay = xpure mkDay parseDay . (integer </> int </> int)
    where mkDay (y :- m :- d :- x) = fromGregorian y m d :- x
          parseDay (day :- x) = let (y, m, d) = toGregorian day in Just $ y :- m :- d :- x

rString :: Boomerang e tok i (T.Text :- o) -> Boomerang e tok i (String :- o)
rString = xmaph T.unpack (Just . T.pack)

sitemap :: Boomerang TextsError [T.Text] r (Sitemap :- r)
sitemap = mconcat
    [ rIndex
    , rYearly . integer
    , rMonthly . integer </> int
    , rDaily . rDay
    , rArticleView . rDay </> rString anyText
    ]
