{-# Language QuasiQuotes #-}
module Views where

import Control.Monad
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.String

import Happstack.Server

import Text.Blaze.Html (Markup)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet)
import Text.Pandoc

import App
import Models

data PageContent = PageContent { pcTitle :: String
                               , pcContent :: Markup
                               }

defaultPage :: PageContent
defaultPage = PageContent { pcTitle = "", pcContent = mempty }

page :: String -> Markup -> PageContent
page title content = defaultPage { pcTitle = title, pcContent = content }

template :: PageContent -> ServerPartT App Response
template page = ok $ toResponse $ [shamlet|
    <html>
        <head>
            <title>#{pcTitle page}
        <body>
            <h1>#{pcTitle page}
            #{pcContent page}
|]

articleListDisplay :: [Article] -> ServerPartT App Response
articleListDisplay articles = template $ page "List" [shamlet|
    $forall article <- articles
        <h2>#{arTitle article}
        <p>#{writeHtml def $ arContent article}
|]

articleDisplay :: Article -> ServerPartT App Response
articleDisplay article = template $ page (arTitle article) [shamlet|
    <p>#{writeHtml def $ arContent article}
|]
