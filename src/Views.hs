{-# Language TemplateHaskell #-}
module Views where

import Control.Monad
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.String

import Happstack.Server

import Text.Blaze.Html (Markup)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamletFile)
import Text.Pandoc
import Text.Pandoc.Walk

import Web.Routes
import Web.Routes.Happstack

import App
import Models
import Routes

data PageContent = PageContent { pcTitle :: String
                               , pcContent :: Markup
                               }

defaultPage :: PageContent
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

page :: String -> Markup -> PageContent
page title content = defaultPage { pcTitle = title, pcContent = content }

template :: PageContent -> RouteT Sitemap (ServerPartT App) Response
template page = ok $ toResponse $ $(shamletFile "templates/base.hamlet")

articleListDisplay :: [Article] -> RouteT Sitemap (ServerPartT App) Response
articleListDisplay articles = template $
    page "List" $(shamletFile "templates/list.hamlet")

articleDisplay :: Article -> RouteT Sitemap (ServerPartT App) Response
articleDisplay article = template $
    page (arTitle article) $(shamletFile "templates/article.hamlet")

arTitle :: Article -> String
arTitle = fromMaybe "Article" . listToMaybe . query extractTitle . arContent
    where extractTitle (Header _ _ [Str title]) = [title]
          extractTitle _ = []

arPreview :: Article -> Pandoc
arPreview = arContent -- TODO
