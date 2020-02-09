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

type AppPart a = ServerPartT App a

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
initAppCache = AppCache <$> initCache

runApp :: AppCache -> AppData -> App a -> IO a
runApp cache app a = runReaderT (evalStateT a cache) app

site :: ServerPartT App Response
site = do
    address <- lift $ asks appAddress
    appDir <- lift $ asks appDirectory
    let staticDir = appDir </> "static"
    let staticSite = serveDirectory DisableBrowsing ["index.html"] staticDir
    let mainSite = do
                       method GET
                       uriRest $ \uri -> case parseURL (T.pack uri) of
                                             Nothing -> mzero
                                             Just route -> handler route
    mainSite `mplus` staticSite

handler :: Sitemap -> AppPart Response
handler route =
    case route of
        Index -> index
        ArticleView d s -> article d s
        MetaView s f -> meta s f
        Feed lang -> feedIndex lang
        SiteScript -> siteScript
        PrintStylesheet -> printStylesheet
        CodeStylesheet -> codeStylesheet

index :: AppPart Response
index = articleList $ const True

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

okResponse :: ToMessage a => a -> AppPart Response
okResponse = ok . toResponse

article :: Day -> Text -> AppPart Response
article date slug = do
    language <- languageHeaderM
    a <- onlyOne $ lift $ askFiltered $ byDateSlug date slug
    articleDisplay language a >>= okResponse

articleList :: (Article -> Bool) -> AppPart Response
articleList articleFilter = do
    articles <- lift $ askFiltered articleFilter
    let sorted = sortBy reverseCompare articles
    page <- pageNumber
    let paginated = paginate pageSize page sorted
    language <- languageHeaderM
    articleListDisplay language paginated >>= okResponse

meta :: Text -> Maybe PageFormat -> AppPart Response
meta slug format' = do
    language <- languageHeaderM
    m <- askMeta slug
    case format' of
        Nothing -> metaDisplay language m >>= okResponse
        Just format -> metaExport format language m >>= okResponse

feedIndex :: Language -> AppPart Response
feedIndex language = do
    articles <- lift $ askFiltered (const True)
    let sorted = sortBy reverseCompare articles
    feedDisplay language sorted >>= okResponse

siteScript :: AppPart Response
siteScript = okResponse renderSiteScript

printStylesheet :: AppPart Response
printStylesheet = okResponse renderPrintStylesheet

codeStylesheet :: AppPart Response
codeStylesheet = okResponse renderCodeStylesheet
