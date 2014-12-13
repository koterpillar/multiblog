{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Views where

import qualified Control.Arrow as A
import Control.Monad

import Data.Maybe
import Data.Monoid
import Data.Time

import Text.Blaze.Html (Markup)
import Text.Hamlet
import Text.Pandoc
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

template :: (MonadRoute m, URL m ~ Sitemap) => PageContent (URL m) -> m Markup
template page = render $(hamletFile "templates/base.hamlet")

articleListDisplay :: (MonadRoute m, URL m ~ Sitemap) => LanguagePreference -> [Article] -> m Markup
articleListDisplay lang articles = template $
    mkPage "List" $(hamletFile "templates/list.hamlet")

articleDisplay :: (MonadRoute m, URL m ~ Sitemap) => LanguagePreference -> Article -> m Markup
articleDisplay lang article = template $
    mkPage (arTitle lang article) $(hamletFile "templates/article.hamlet")

arTitle :: LanguagePreference -> Article -> String
arTitle lang = fromMaybe "Article" . listToMaybe . query extractTitle . arLangContent lang
    where extractTitle (Header _ _ [Str title]) = [title]
          extractTitle _ = []

arLangContent :: LanguagePreference -> Article -> Pandoc
arLangContent lang = fromJust . matchLanguage lang . arContent

arPreview :: LanguagePreference -> Article -> Pandoc
arPreview lang = pandocFilter (take 2 . filter isTextual) . arLangContent lang

pandocFilter :: ([Block] -> [Block]) -> Pandoc -> Pandoc
pandocFilter f (Pandoc m bs) = Pandoc m (f bs)

isTextual :: Block -> Bool
isTextual Header{} = False
isTextual _ = True
