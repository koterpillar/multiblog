{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import Data.Time

import Happstack.Server

import Web.Routes
import Web.Routes.Boomerang
import Web.Routes.Happstack

import App
import Language
import Models
import Routes
import Views

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
languageHeaderM = do
    request <- askRq
    let header = getHeader "Accept-Language" request
    return $ languageHeader $ liftM B.unpack header

html :: ToMessage a => a -> AppPart Response
html = ok . toResponse

article :: Day -> String -> AppPart Response
article date slug = do
    language <- languageHeaderM
    -- TODO: getOne
    articles <- lift $ getFiltered $ byDateSlug date slug
    case articles of
        [a] -> html $ articleDisplay language a
        _ -> mzero

articleList :: (Article -> Bool) -> AppPart Response
articleList articleFilter = do
    articles <- lift $ getFiltered articleFilter
    language <- languageHeaderM
    html $ articleListDisplay language articles
