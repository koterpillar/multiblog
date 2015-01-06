{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Views where

import qualified Control.Arrow as A
import Control.Monad
import Control.Monad.State

import Data.Maybe
import Data.Monoid
import Data.Time

import Text.Blaze.Html (Markup)
import Text.Hamlet
import Text.Pandoc hiding (Meta)
import Text.Pandoc.Walk

import Web.Routes

import Language
import Models
import Routes

data PageContent a = PageContent { pcTitle   :: String
                                 , pcContent :: HtmlUrl a
                                 }

articleLink :: Article -> Sitemap
articleLink a = ArticleView (utctDay $ arAuthored a) (arSlug a)

metaLink :: Meta -> Sitemap
metaLink = MetaView . mtSlug

defaultPage :: PageContent a
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

mkPage :: String -> HtmlUrl a -> PageContent a
mkPage title content = defaultPage { pcTitle = title
                                   , pcContent = content
                                   }

convRender :: (url -> [(a, Maybe b)] -> c)  -> url -> [(a, b)] -> c
convRender maybeF url params = maybeF url $ map (A.second Just) params

render :: MonadRoute m => HtmlUrl (URL m) -> m Markup
render html = do
    route <- liftM convRender askRouteFn
    return $ html route

template :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> PageContent (URL m) -> m Markup
template lang page = do
    -- TODO: need to be able to get any meta inside
    about <- getMeta "about"
    render $(hamletFile "templates/base.hamlet")

articleListDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> [Article] -> m Markup
articleListDisplay lang articles = template lang $
    mkPage "List" $(hamletFile "templates/list.hamlet")

articleDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> Article -> m Markup
articleDisplay lang article = template lang $
    mkPage (langTitle lang article) $(hamletFile "templates/article.hamlet")

metaDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    LanguagePreference -> Meta -> m Markup
metaDisplay lang meta = template lang $
    mkPage (langTitle lang meta) $(hamletFile "templates/meta.hamlet")

langTitle :: HasContent a => LanguagePreference -> a -> String
langTitle lang = fromMaybe "untitled" . listToMaybe . query extractTitle . langContent lang
    where extractTitle (Header _ _ title) = [inlineToStr title]
          extractTitle _ = []

-- TODO: Might be a better way to do this in Pandoc
inlineToStr :: [Inline] -> String
inlineToStr inline = writePlain def $ Pandoc undefined [Plain inline]

langContent :: HasContent a => LanguagePreference -> a -> Pandoc
langContent lang = fromJust . matchLanguage lang . getContent

arPreview :: LanguagePreference -> Article -> Pandoc
arPreview lang = pandocFilter (take 2 . filter isTextual) . langContent lang

pandocFilter :: ([Block] -> [Block]) -> Pandoc -> Pandoc
pandocFilter f (Pandoc m bs) = Pandoc m (f bs)

isTextual :: Block -> Bool
isTextual Header{} = False
isTextual _ = True
