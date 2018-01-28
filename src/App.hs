{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Applicative (optional)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import Data.Functor.Identity
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time

import Happstack.Server

import System.Directory
import System.Environment
import System.FilePath

import Web.Routes
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack

import Cache
import Import
import Models
import Routes
import Types.Content
import Types.Language
import Utils
import Views
import Views.Export
import Views.Feed

type App = StateT AppCache (ReaderT AppData IO)

type AppPart a = RouteT Sitemap (ServerPartT App) a

loadApp ::
       String -- ^ directory to load from
    -> T.Text -- ^ site address
    -> Bool -- ^ whether the address was explicitly specified
    -> IO AppData
loadApp dataDirectory address isRealAddress = do
    app <- runExceptT $ loadFromDirectory dataDirectory
    case app of
        Left err -> error err
        Right appState ->
            return
                appState {appAddress = address, appRealAddress = isRealAddress}

-- | Application address, and whether it's specified explicitly
siteAddress :: IO (T.Text, Bool)
siteAddress = do
    addr <- fmap T.pack <$> lookupEnv "SITE_URL"
    return $
        case addr of
            Just realAddr -> (realAddr, True)
            Nothing -> ("http://localhost:8000", False)

-- | Application directory to use
getAppDirectory :: IO FilePath
getAppDirectory = do
    val <- lookupEnv "CONTENT_DIRECTORY"
    case val of
        Just contentDir -> return contentDir
        Nothing -> getCurrentDirectory

loadAppDefault :: IO AppData
loadAppDefault = do
    (address, isRealAddress) <- siteAddress
    directory <- getAppDirectory
    loadApp directory address isRealAddress

initAppCache :: IO AppCache
initAppCache = do
    pdfCache <- initCache
    return $ AppCache pdfCache

runApp :: AppCache -> AppData -> App a -> IO a
runApp cache app a = runReaderT (evalStateT a cache) app

site :: ServerPartT App Response
site = do
    address <- lift $ asks appAddress
    appDir <- lift $ asks appDirectory
    let routedSite = boomerangSiteRouteT handler sitemap
    let staticDir = appDir </> "static"
    let staticSite = serveDirectory DisableBrowsing ["index.html"] staticDir
    implSite address "" routedSite `mplus` staticSite

-- Run an action in application routing context
runRoute :: RouteT Sitemap m a -> m a
runRoute act
    -- Supply a known good URL (root) to run the site,
    -- producing the result of the given action
 =
    let (Right res) = runSite "" (boomerangSiteRouteT (const act) sitemap) []
    in res

parseRoute :: T.Text -> Either String Sitemap
parseRoute =
    fmap runIdentity . runSite "" (boomerangSiteRouteT pure sitemap) . segments
  where
    segments = decodePathInfo . T.encodeUtf8

handler :: Sitemap -> AppPart Response
handler route =
    case route of
        Index -> index
        Yearly y -> yearlyIndex y
        Monthly y m -> monthlyIndex y m
        Daily d -> dailyIndex d
        ArticleView d s -> article d s
        MetaView s f -> meta s f
        Feed lang -> feedIndex lang
        SiteScript -> siteScript
        PrintStylesheet -> printStylesheet
        CodeStylesheet -> codeStylesheet

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
    let header = B.unpack <$> getHeader "Accept-Language" request
    param <- optional $ look "lang"
    cookie <- optional $ lookCookieValue "lang"
    let langValue = listToMaybe $ catMaybes [param, cookie, header]
    return $ languageHeader langValue

pageNumber :: AppPart PageNumber
pageNumber = do
    page <- optional $ look "page"
    return $ fromMaybe 1 $ page >>= readM

html :: ToMessage a => a -> AppPart Response
html = ok . toResponse

article :: Day -> Text -> AppPart Response
article date slug = do
    language <- languageHeaderM
    a <- onlyOne $ lift $ askFiltered $ byDateSlug date slug
    articleDisplay language a >>= html

articleList :: (Article -> Bool) -> AppPart Response
articleList articleFilter = do
    articles <- lift $ askFiltered articleFilter
    let sorted = sortBy reverseCompare articles
    page <- pageNumber
    let paginated = paginate pageSize page sorted
    language <- languageHeaderM
    articleListDisplay language paginated >>= html

meta :: Text -> Maybe PageFormat -> AppPart Response
meta slug format' = do
    let format = fromMaybe Html format'
    language <- languageHeaderM
    m <- askMeta slug
    case format of
        Html -> metaDisplay language m >>= html
        _ -> metaExport format language m >>= html

feedIndex :: Language -> AppPart Response
feedIndex language = do
    articles <- lift $ askFiltered (const True)
    let sorted = sortBy reverseCompare articles
    feedDisplay language sorted >>= html

-- Override content type on any response
data WithContentType r =
    WithContentType String
                    r

instance ToMessage r => ToMessage (WithContentType r) where
    toResponse (WithContentType ct r) =
        setHeaderBS (B.pack "Content-Type") (B.pack ct) $ toResponse r

asCss :: r -> WithContentType r
asCss = WithContentType "text/css"

siteScript :: AppPart Response
siteScript = renderSiteScript >>= html

printStylesheet :: AppPart Response
printStylesheet = renderPrintStylesheet >>= html . asCss

codeStylesheet :: AppPart Response
codeStylesheet = renderCodeStylesheet >>= html . asCss
