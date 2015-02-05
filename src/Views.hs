{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Views where

import qualified Control.Arrow as A
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time

import Text.Blaze.Html (Markup)
import Text.Hamlet
import Text.Pandoc hiding (Meta)
import Text.Pandoc.Walk

import Web.Routes

import Language
import Models
import Routes

data PageContent a = PageContent { pcTitle   :: Maybe String
                                 , pcContent :: HtmlUrl a
                                 }

class Linkable a where
    link :: a -> Sitemap

instance Linkable Article where
    link a = ArticleView (utctDay $ arAuthored a) (arSlug a)

instance Linkable Meta where
    link = MetaView . mtSlug

defaultPage :: PageContent a
defaultPage = PageContent { pcTitle = Nothing, pcContent = mempty }

mkPage :: Maybe String -> HtmlUrl a -> PageContent a
mkPage title content = defaultPage { pcTitle = title
                                   , pcContent = content
                                   }

convRender :: (url -> [(a, Maybe b)] -> c)  -> url -> [(a, b)] -> c
convRender maybeF url params = maybeF url $ map (A.second Just) params

render :: MonadRoute m => HtmlUrl (URL m) -> m Markup
render html = do
    route <- liftM convRender askRouteFn
    return $ html route

getLangStringFn :: MonadState AppState m => LanguagePreference -> m (String -> String)
getLangStringFn lang = do
    strings <- gets appStrings
    let fn str = fromMaybe str $ M.lookup str strings >>= matchLanguage lang
    return fn

getLangString :: MonadState AppState m => LanguagePreference -> String -> m String
getLangString lang str = getLangStringFn lang >>= (\fn -> return $ fn str)

template :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> PageContent (URL m) -> m Markup
template lang page = do
    -- TODO: need to be able to get any meta inside
    about <- getMeta "about"
    langString <- getLangStringFn lang
    render $(hamletFile "templates/base.hamlet")

articleListDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> [Article] -> m Markup
articleListDisplay lang articles = do
    articlesContent <- mapM (linkedContent lang) articles
    template lang $
        mkPage Nothing $(hamletFile "templates/list.hamlet")

articleDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> Article -> m Markup
articleDisplay lang article = template lang $
    mkPage (Just $ langTitle lang article) $(hamletFile "templates/article.hamlet")

metaDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> Meta -> m Markup
metaDisplay lang meta = template lang $
    mkPage (Just $ langTitle lang meta) $(hamletFile "templates/meta.hamlet")

langTitle :: HasContent a => LanguagePreference -> a -> String
langTitle lang = fromMaybe "untitled" . listToMaybe . query extractTitle . langContent lang
    where extractTitle (Header _ _ title) = [inlineToStr title]
          extractTitle _ = []

-- TODO: Might be a better way to do this in Pandoc
inlineToStr :: [Inline] -> String
inlineToStr inline = writePlain def $ Pandoc undefined [Plain inline]

langContent :: HasContent a => LanguagePreference -> a -> Pandoc
langContent lang = fromJust . matchLanguage lang . getContent

-- Modify the content to have a link to itself and have no anchors
linkedContent :: (HasContent a, Linkable a, MonadRoute m, URL m ~ Sitemap)
              => LanguagePreference
              -> a
              -> m Pandoc
linkedContent lang a = do
    routeFn <- askRouteFn
    let target = T.unpack $ routeFn (link a) []
    return $ linkedHeader target $ langContent lang a

-- Modify the first header to be a link to a given place
-- and remove all anchors from headers
linkedHeader :: String -> Pandoc -> Pandoc
linkedHeader target doc = evalState (walkM linkHeader doc) True
    where linkHeader :: Block -> State Bool Block
          linkHeader (Header n _ text) = do
              -- note if this is the first header
              isFirst <- get
              put False
              -- make the first header a link
              let text' = if isFirst then [Link text (target, "")] else text
              -- remove anchors
              return $ Header n ("",[],[]) text'
          linkHeader x = return x
