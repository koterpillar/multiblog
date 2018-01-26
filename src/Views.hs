{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Views where

import qualified Control.Arrow as A
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Default.Class
import Data.LanguageCodes
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
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

class Linkable a  where
    link :: a -> Sitemap

instance Linkable Sitemap where
    link = id

instance Linkable Article where
    link a = ArticleView (utctDay $ arAuthored a) (arSlug a)

instance Linkable Meta where
    link m = MetaView (mtSlug m) Nothing

type AppRoute m = (MonadRoute m, URL m ~ Sitemap)

data PageContent = PageContent
    { pcTitle :: Maybe Text
    , pcLayout :: Layout
    , pcContent :: HtmlUrl Sitemap
    }

instance Default PageContent where
    def =
        PageContent
        {pcTitle = Nothing, pcLayout = BaseLayout, pcContent = mempty}

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
    route <- fmap convRender askRouteFn
    return $ html route

askLangStringFn
    :: MonadReader AppData m
    => LanguagePreference -> m (Text -> Text)
askLangStringFn lang = do
    strings <- asks appStrings
    let fn str = fromMaybe str $ M.lookup str strings >>= matchLanguage lang
    pure fn

askLangString
    :: MonadReader AppData m
    => LanguagePreference -> Text -> m Text
askLangString lang str = askLangStringFn lang >>= (\fn -> pure $ fn str)

linkTitle
    :: (MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Link -> m Text
linkTitle lang (ExternalLink url titles) =
    pure $ fromMaybe url $ matchLanguage lang titles
linkTitle lang (MetaLink slug) = do
    meta <- askMeta slug
    pure $ langTitle lang meta

linkDestination
    :: (AppRoute m, MonadReader AppData m, MonadPlus m)
    => Link -> m Text
linkDestination (ExternalLink url _) = pure url
linkDestination (MetaLink slug) = do
    meta <- askMeta slug
    route <- askRouteFn
    pure $ route (link meta) []

template
    :: (AppRoute m, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> PageContent -> m Markup
template lang page = do
    links <- asks appLinks
    linkTitleUrls <-
        forM links $ \l -> do
            title <- linkTitle lang l
            destination <- linkDestination l
            return (title, destination)
    allLangs <- asks allLanguages
    langString <- askLangStringFn lang
    analyticsIDs <- asks appAnalytics
    let analytics = $(hamletFile "templates/analytics.hamlet")
    case pcLayout page of
        BaseLayout -> render $(hamletFile "templates/base.hamlet")
        PresentationLayout -> render $(hamletFile "templates/base_presentation.hamlet")

articleListDisplay
    :: (AppRoute m, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Paginated Article -> m Markup
articleListDisplay lang articles = do
    articlesContent <- mapM (linkedContent lang) (pageItems articles)
    langString <- askLangStringFn lang
    template lang $ def {pcContent = $(hamletFile "templates/list.hamlet")}

articleDisplay
    :: (AppRoute m, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Article -> m Markup
articleDisplay lang article =
    template lang $
    def
    { pcTitle = Just $ langTitle lang article
    , pcContent = $(hamletFile "templates/article.hamlet")
    }

metaDisplay
    :: (AppRoute m, MonadReader AppData m, MonadPlus m)
    => LanguagePreference -> Meta -> m Markup
metaDisplay lang meta =
    template lang $
    def
    { pcTitle = Just $ langTitle lang meta
    , pcLayout = mtLayout meta
    , pcContent = content
    }
  where
    content =
        case mtLayout meta of
            BaseLayout -> $(hamletFile "templates/meta.hamlet")
            PresentationLayout -> $(hamletFile "templates/meta_presentation.hamlet")

-- Generate a link to some content
linkTo
    :: (Linkable a, AppRoute m)
    => a -> m Text
linkTo a = do
    routeFn <- askRouteFn
    pure $ routeFn (link a) []

-- Modify the content to have a link to itself and have no anchors
linkedContent
    :: (HasContent a, Linkable a, AppRoute m)
    => LanguagePreference -> a -> m Pandoc
linkedContent lang a = do
    target <- linkTo a
    pure $ linkedHeader target $ langContent lang a

-- Modify the first header to be a link to a given place
-- and remove all anchors from headers
linkedHeader :: Text -> Pandoc -> Pandoc
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
                    then [Link nullAttr text (T.unpack target, "")]
                    else text
        -- remove anchors
        return $ Header n ("", [], []) text'
    linkHeader x = return x

renderSiteScript
    :: MonadRoute m
    => m TL.Text
renderSiteScript = do
    route <- fmap convRender askRouteFn
    return $ renderJavascriptUrl route $(juliusFile "templates/site.julius")

renderPrintStylesheet
    :: MonadRoute m
    => m TL.Text
renderPrintStylesheet = do
    route <- fmap convRender askRouteFn
    return $ renderCssUrl route $(luciusFile "templates/print.lucius")

-- | Remark can only render the pipe tables. Disable the other kinds
remarkOptions :: WriterOptions
remarkOptions =
    def
    { writerExtensions =
          foldr disableExtension (writerExtensions def) remarkUnsupported
    }
  where
    remarkUnsupported =
        [ Ext_simple_tables
        , Ext_multiline_tables
        , Ext_grid_tables
        , Ext_fenced_code_attributes
        ]

-- | Remark relies on "---" being a slide separator, Pandoc makes it into a
-- horizontal line
fixSlideSeparators :: Pandoc -> Pandoc
fixSlideSeparators = walk fixSeparators
  where
    fixSeparators :: Block -> Block
    fixSeparators HorizontalRule = Para [Str "---"]
    fixSeparators x = x
