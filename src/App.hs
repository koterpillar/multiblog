{-# LANGUAGE OverloadedStrings #-}
module App where

import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Time

import Happstack.Server

import Web.Routes
import Web.Routes.Boomerang
import Web.Routes.Happstack

import Import
import Language
import Models
import Routes
import Utils
import Views

-- TODO: This should be a ReaderT
type App = StateT AppState IO

type AppPart a = RouteT Sitemap (ServerPartT App) a

loadApp :: IO AppState
loadApp = do
    app <- loadFromDirectory "content"
    case app of
        Left err -> error err
        Right appState -> return appState

runApp :: AppState -> App a -> IO a
runApp app a = evalStateT a app

site :: Site Sitemap (ServerPartT App Response)
site = boomerangSiteRouteT handler sitemap

siteHandler :: String -> ServerPartT App Response
siteHandler address = implSite (T.pack address) "" site

handler :: Sitemap -> AppPart Response
handler route = case route of
    Index -> index
    Yearly y -> yearlyIndex y
    Monthly y m -> monthlyIndex y m
    Daily d -> dailyIndex d
    ArticleView d s -> article d s
    MetaView s -> meta s

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
    m <- getMeta slug
    metaDisplay language m >>= html
