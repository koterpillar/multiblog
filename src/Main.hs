module Main where

import Control.Monad
import Control.Monad.State

import Data.Time

import Happstack.Server

import Web.Routes

import App
import Models
import Routes
import Views


main :: IO ()
main = simpleHTTP' runApp nullConf $ msum [ mzero
                                          , index
                                          , yearlyIndex
                                          , monthlyIndex
                                          , dailyIndex
                                          , article
                                          ]

route :: Sitemap -> RouteT Sitemap (ServerPartT App) Response
route url = case url of
    VHome -> index
    VYearly y -> yearlyIndex y
    VMonthly y m -> monthlyIndex y m
    VDaily day -> dailyIndex day
    VArticle day slug -> article day slug

index :: RouteT Sitemap (ServerPartT App) Response
index = articleList $ const True

yearlyIndex :: Integer -> RouteT Sitemap (ServerPartT App) Response
yearlyIndex year = articleList $ byYear year

monthlyIndex :: Integer -> Int -> RouteT Sitemap (ServerPartT App) Response
monthlyIndex year month = articleList $ byYearMonth year month

dailyIndex :: Day -> RouteT Sitemap (ServerPartT App) Response
dailyIndex day = articleList $ byDate undefined

article :: Day -> String -> RouteT Sitemap (ServerPartT App) Response
article day slug = do
    article <- getOne $ byDateSlug undefined slug
    articleDisplay article

articleList :: (Article -> Bool) -> RouteT Sitemap (ServerPartT App) Response
articleList articleFilter = do
    articles <- getFiltered articleFilter
    articleListDisplay articles
