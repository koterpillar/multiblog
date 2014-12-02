{-# LANGUAGE TemplateHaskell #-}
module Views where

import Control.Monad
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text.Lazy (Text, pack)
import Data.Time

import Text.Blaze.Html (Markup)
import Text.Hamlet (hamletFile)
import Text.Pandoc
import Text.Pandoc.Walk

import App
import Language
import Models

data PageContent = PageContent { pcTitle   :: String
                               , pcContent :: Markup
                               }

-- TODO: this has to be kept in sync with the routes in Main
data Route = RIndex
           | RYearly Integer
           | RMonthly Integer Int
           | RDaily Day
           | RArticle Article

type Router = Route -> [(Text, Text)] -> Text

router :: Router
router RIndex _ = pack "/"
-- TODO: yearly, monthly, daily
router (RArticle article) _ = pack $ "/" ++ show y
                                  ++ "/" ++ show m
                                  ++ "/" ++ show d
                                  ++ "/" ++ slug
    where slug = arSlug article
          (y, m, d) = toGregorian $ utctDay $ arAuthored article

defaultPage :: PageContent
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

page :: String -> (Router -> Markup) -> PageContent
page title content = defaultPage { pcTitle = title
                                 , pcContent = content router }

template :: PageContent -> Markup
template page = $(hamletFile "templates/base.hamlet") router

articleListDisplay :: LanguagePreference -> [Article] -> Markup
articleListDisplay lang articles = template $
    page "List" $(hamletFile "templates/list.hamlet")

articleDisplay :: LanguagePreference -> Article -> Markup
articleDisplay lang article = template $
    page (arTitle lang article) $(hamletFile "templates/article.hamlet")

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
