{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Prelude hiding ((.))

import Control.Category (Category((.)))
import Control.Monad
import Control.Monad.State

import Data.Monoid
import qualified Data.Text as T
import Data.Time

import Happstack.Server

import Text.Boomerang.TH (makeBoomerangs)

import Web.Routes
import Web.Routes.Boomerang
import Web.Routes.Happstack

import App
import Language
import Models
import Views

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

type AppPart a = RouteT Sitemap (ServerPartT App) a

handler :: Sitemap -> AppPart Response
handler route = case route of
    Index -> index
    Yearly y -> yearlyIndex y
    Monthly y m -> monthlyIndex y m
    Daily d -> dailyIndex d
    ArticleView d s -> article d s

site :: Site Sitemap (ServerPartT App Response)
site = boomerangSiteRouteT handler sitemap

main :: IO ()
main = simpleHTTP' runApp nullConf $
    implSite "http://example.org" "" site

index :: AppPart Response
index = articleList $ const True

yearlyIndex :: Integer -> AppPart Response
yearlyIndex = articleList . byYear

monthlyIndex :: Integer -> Int -> AppPart Response
monthlyIndex year month = articleList $ byYearMonth year month

dailyIndex :: Day -> AppPart Response
dailyIndex date = articleList $ byDate date

languageHeaderM :: AppPart LanguagePreference
-- TODO
-- languageHeaderM = liftM (languageHeader . liftM unpack) $ header "Accept-Language"
languageHeaderM = return $ languageHeader Nothing

-- TODO
renderHtml = undefined

article :: Day -> String -> AppPart Response
article date slug = do
    language <- languageHeaderM
    -- TODO: getOne
    articles <- lift $ getFiltered $ byDateSlug date slug
    case articles of
        [a] -> renderHtml $ articleDisplay language a
        _ -> mzero

articleList :: (Article -> Bool) -> AppPart Response
articleList articleFilter = do
    articles <- lift $ getFiltered articleFilter
    language <- languageHeaderM
    renderHtml $ articleListDisplay language articles
