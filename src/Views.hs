{-# LANGUAGE TemplateHaskell #-}
module Views where

import Data.Maybe
import Data.Monoid

import Text.Blaze.Html (Markup)
import Text.Hamlet (hamletFile)
import Text.Pandoc
import Text.Pandoc.Walk

import Language
import Models

data PageContent = PageContent { pcTitle   :: String
                               , pcContent :: Markup
                               }

-- TODO
data ZZRouter = ZZRouter ZZRouter

defaultPage :: PageContent
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

mkPage :: String -> (ZZRouter -> Markup) -> PageContent
mkPage title content = defaultPage { pcTitle = title
                                 , pcContent = content undefined }

template :: PageContent -> Markup
template page = $(hamletFile "templates/base.hamlet") undefined

articleListDisplay :: LanguagePreference -> [Article] -> Markup
articleListDisplay lang articles = template $
    mkPage "List" $(hamletFile "templates/list.hamlet")

articleDisplay :: LanguagePreference -> Article -> Markup
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
