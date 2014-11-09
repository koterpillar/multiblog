{-# Language QuasiQuotes #-}
module Views where

import Control.Monad
import Control.Monad.State

import Data.ByteString.Char8 (ByteString)
import Data.String

import Happstack.Server

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet)

import App
import Models

articleListDisplay :: [Article] -> ServerPartT App Response
articleListDisplay articles = ok $ toResponse $ [shamlet|
    <html>
        <head>
            <title>List
        <body>
            <h1>List
            $forall article <- articles
                <h2>#{arTitle article}
                <p>#{arContent article}
|]

articleDisplay :: Article -> ServerPartT App Response
articleDisplay article = ok $ toResponse $ [shamlet|
    <html>
        <head>
            <title>#{arTitle article}
        <body>
            <h1>#{arTitle article}
            <p>#{arContent article}
|]
