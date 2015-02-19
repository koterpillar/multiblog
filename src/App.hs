{-# LANGUAGE OverloadedStrings #-}
module App where

import Control.Applicative (optional)
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
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
import Views.Feed

-- TODO: This should be a ReaderT
type App = StateT AppState IO

type AppPart a = RouteT Sitemap (ServerPartT App) a

loadApp :: String -- directory to load from
        -> String -- site address
        -> IO AppState
loadApp dataDirectory siteAddress = do
    app <- loadFromDirectory dataDirectory
    case app of
        Left err -> error err
        Right appState -> return appState { appAddress = siteAddress }

runApp :: AppState -> App a -> IO a
runApp app a = evalStateT a app

site :: ServerPartT App Response
site = do
    address <- lift $ gets appAddress
    appDir <- lift $ gets appDirectory
    let routedSite = boomerangSiteRouteT handler sitemap
    let staticSite = serveDirectory DisableBrowsing [] $ appDir ++ "/static"
    implSite (T.pack address) "" routedSite `mplus` staticSite

handler :: Sitemap -> AppPart Response
handler route = case route of
    Index -> index
    Yearly y -> yearlyIndex y
    Monthly y m -> monthlyIndex y m
    Daily d -> dailyIndex d
    ArticleView d s -> article d s
    MetaView s -> meta s
    Feed lang -> feedIndex lang
    SiteScript -> siteScript

index :: AppPart Response
index = articleList $ const True

yearlyIndex :: Integer -> AppPart Response
yearlyIndex = articleList . byYear

monthlyIndex :: Integer -> Int -> AppPart Response
monthlyIndex year month = articleList $ byYearMonth year month

dailyIndex :: Day -> AppPart Response
dailyIndex = articleList . byDate

-- Find the most relevant language preference in a request
-- Includes: explicit GET parameter, cookie and Accept-Language header
languageHeaderM :: AppPart LanguagePreference
languageHeaderM = do
    request <- askRq
    let header = liftM B.unpack $ getHeader "Accept-Language" request
    param <- optional $ look "lang"
    cookie <- optional $ lookCookieValue "lang"
    let langValue = listToMaybe $ catMaybes [param, cookie, header]
    return $ languageHeader langValue

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
    let sorted = sortBy reverseCompare articles
    language <- languageHeaderM
    articleListDisplay language sorted >>= html

meta :: String -> AppPart Response
meta slug = do
    language <- languageHeaderM
    m <- getMeta slug
    metaDisplay language m >>= html

feedIndex :: Language -> AppPart Response
feedIndex language = do
    articles <- lift $ getFiltered (const True)
    let sorted = sortBy reverseCompare articles
    feedDisplay language sorted >>= html

siteScript :: AppPart Response
siteScript = renderSiteScript >>= html
