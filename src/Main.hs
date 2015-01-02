{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Text as T
import Data.Time

import Happstack.Server

import System.Environment

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
    MetaView s -> meta s

site :: Site Sitemap (ServerPartT App Response)
site = boomerangSiteRouteT handler sitemap

siteAddress :: IO String
siteAddress = do
    addr <- lookupEnv "SITE_URL"
    return $ fromMaybe "http://localhost:8000" addr

main :: IO ()
main = do
    address <- siteAddress
    simpleHTTP' runApp nullConf $ implSite (T.pack address) "" site

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

onlyOne :: MonadPlus m => m [a] -> m a
onlyOne action = do
    ms <- action
    case ms of
        [m] -> return m
        _ -> mzero

article :: Day -> String -> AppPart Response
article date slug = do
    language <- languageHeaderM
    -- TODO: onlyOne
    a <- onlyOne $ lift $ getFiltered $ byDateSlug date slug
    articleDisplay language a >>= html

articleList :: (Article -> Bool) -> AppPart Response
articleList articleFilter = do
    articles <- lift $ getFiltered articleFilter
    language <- languageHeaderM
    articleListDisplay language articles >>= html

meta :: String -> AppPart Response
meta slug = do
    language <- languageHeaderM
    m <- onlyOne $ lift $ gets $ filter (bySlug slug) . appMeta
    metaDisplay language m >>= html
