{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Views where

import qualified Control.Arrow as A
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.LanguageCodes
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time

import Text.Blaze.Html (Markup)
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Text.Pandoc hiding (Meta, Reader)
import Text.Pandoc.Walk

import Web.Routes

import Models
import Types.Content
import Types.Language
import Routes

data PageContent = PageContent
    { pcTitle :: Maybe String
    , pcContent :: HtmlUrl Sitemap
    }

class Linkable a  where
    link :: a -> Sitemap

instance Linkable Sitemap where
    link = id

instance Linkable Article where
    link a = ArticleView (utctDay $ arAuthored a) (arSlug a)

instance Linkable Meta where
    link m = MetaView (mtSlug m) Nothing

defaultPage :: PageContent
defaultPage =
    PageContent
    { pcTitle = Nothing
    , pcContent = mempty
    }

mkPage :: Maybe String -> HtmlUrl Sitemap -> PageContent
mkPage title content =
    defaultPage
    { pcTitle = title
    , pcContent = content
    }

type PageNumber = Int

data Paginated a = Paginated { pagePrev :: Maybe PageNumber
                             , pageItems :: [a]
                             , pageNext :: Maybe PageNumber
                             }
    deriving (Eq, Show)

pageSize :: Int
pageSize = 10

paginate :: Int -> PageNumber -> [a] -> Paginated a
paginate size page allItems = Paginated prev items next
   where
       prev | page == 1 = Nothing
            | otherwise = Just (page - 1)
       offsetItems = drop ((page - 1) * size) allItems
       (items, rest) = splitAt size offsetItems
       next | null rest = Nothing
            | otherwise = Just (page + 1)

convRender :: (url -> [(a, Maybe b)] -> c) -> url -> [(a, b)] -> c
convRender maybeF url params = maybeF url $ map (A.second Just) params

render
    :: MonadRoute m
    => HtmlUrl (URL m) -> m Markup
render html = do
    route <- liftM convRender askRouteFn
    return $ html route

askLangStringFn
    :: MonadReader AppData m
    => LanguagePreference -> m (String -> String)
askLangStringFn lang = do
    strings <- asks appStrings
    let fn str = fromMaybe str $ M.lookup str strings >>= matchLanguage lang
    return fn

askLangString
    :: MonadReader AppData m
    => LanguagePreference -> String -> m String
askLangString lang str = askLangStringFn lang >>= (\fn -> return $ fn str)

linkTitle
    :: (MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Link -> m String
linkTitle lang (ExternalLink url titles) =
    return $ fromMaybe url $ matchLanguage lang titles
linkTitle lang (MetaLink slug) = do
    meta <- askMeta slug
    return $ langTitle lang meta

linkDestination
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadPlus m)
    => Link -> m String
linkDestination (ExternalLink url _) = return url
linkDestination (MetaLink slug) = do
    meta <- askMeta slug
    route <- askRouteFn
    return $ T.unpack $ route (link meta) []

template
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> PageContent -> m Markup
template lang page = do
    links <- asks appLinks
    linkTitleUrls <-
        forM links $
        \l -> do
            title <- linkTitle lang l
            destination <- linkDestination l
            return (title, destination)
    allLangs <- asks allLanguages
    langString <- askLangStringFn lang
    analyticsIDs <- asks appAnalytics
    let analytics = $(hamletFile "templates/analytics.hamlet")
    render $(hamletFile "templates/base.hamlet")

articleListDisplay
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Paginated Article -> m Markup
articleListDisplay lang articles = do
    articlesContent <- mapM (linkedContent lang) (pageItems articles)
    langString <- askLangStringFn lang
    template lang $ mkPage Nothing $(hamletFile "templates/list.hamlet")

articleDisplay
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Article -> m Markup
articleDisplay lang article =
    template lang $
    mkPage
        (Just $ langTitle lang article)
        $(hamletFile "templates/article.hamlet")

metaDisplay
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Meta -> m Markup
metaDisplay lang meta =
    template lang $
    mkPage (Just $ langTitle lang meta) $(hamletFile "templates/meta.hamlet")

-- Generate a link to some content
linkTo
    :: (Linkable a, MonadRoute m, URL m ~ Sitemap)
    => a -> m String
linkTo a = do
    routeFn <- askRouteFn
    return $ T.unpack $ routeFn (link a) []

-- Modify the content to have a link to itself and have no anchors
linkedContent
    :: (HasContent a, Linkable a, MonadRoute m, URL m ~ Sitemap)
    => LanguagePreference -> a -> m Pandoc
linkedContent lang a = do
    target <- linkTo a
    return $ linkedHeader target $ langContent lang a

-- Modify the first header to be a link to a given place
-- and remove all anchors from headers
linkedHeader :: String -> Pandoc -> Pandoc
linkedHeader target doc = evalState (walkM linkHeader doc) True
  where
    linkHeader :: Block -> State Bool Block
    linkHeader (Header n _ text)
               -- note if this is the first header
     = do
        isFirst <- get
        put False
        -- make the first header a link
        let text' =
                if isFirst
                    then [Link nullAttr text (target, "")]
                    else text
        -- remove anchors
        return $ Header n ("", [], []) text'
    linkHeader x = return x

renderSiteScript
    :: MonadRoute m
    => m TL.Text
renderSiteScript = do
    route <- liftM convRender askRouteFn
    return $ renderJavascriptUrl route $(juliusFile "templates/site.julius")

renderPrintStylesheet
    :: MonadRoute m
    => m TL.Text
renderPrintStylesheet = do
    route <- liftM convRender askRouteFn
    return $ renderCssUrl route $(luciusFile "templates/print.lucius")
