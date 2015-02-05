{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Views.Feed where

import Control.Monad.State

import Text.Atom.Feed (Feed, TextContent (..), nullFeed)
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML.Light.Output (showTopElement)

import Web.Routes

import Language
import Models
import Routes
import Views

makeFeed :: MonadState AppState m => Language -> m Feed
makeFeed lang = do
    siteName <- getLangString (singleLanguage lang) "siteName"
    siteId <- gets appAddress
    -- TODO where to get lastUpdated?
    let lastUpdatedStr = "2015-01-01"
    return $ nullFeed siteId (TextString siteName) lastUpdatedStr

feedDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    Language -> [Article] -> m String
feedDisplay lang _ = do
    feed <- makeFeed lang
    return $ showTopElement $ xmlFeed feed
