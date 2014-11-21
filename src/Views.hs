{-# Language TemplateHaskell #-}
module Views where

import Control.Monad
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.String

import Text.Blaze.Html (Markup)
import Text.Hamlet (shamletFile)
import Text.Pandoc
import Text.Pandoc.Walk

import App
import Models

data PageContent = PageContent { pcTitle :: String
                               , pcContent :: Markup
                               }

defaultPage :: PageContent
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

page :: String -> Markup -> PageContent
page title content = defaultPage { pcTitle = title, pcContent = content }

template :: PageContent -> Markup
template page = $(shamletFile "templates/base.hamlet")

articleListDisplay :: Language -> [Article] -> Markup
articleListDisplay = undefined
-- articleListDisplay lang articles = template $
--     page "List" $(shamletFile "templates/list.hamlet")

articleDisplay :: Language -> Article -> Maybe Markup
articleDisplay = undefined
-- articleDisplay lang article = template $
--     page (arTitle lang article) $(shamletFile "templates/article.hamlet")

arTitle :: Language -> Article -> String
arTitle lang = fromMaybe "Article" . listToMaybe . query extractTitle . arLangContent lang
    where extractTitle (Header _ _ [Str title]) = [title]
          extractTitle _ = []

arPreview :: Language -> Article -> Maybe Pandoc
arPreview = arLangContent -- TODO
