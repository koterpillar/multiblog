{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Views.Feed where

import Control.Monad.State

import Data.Time

import Text.Atom.Feed (TextContent (..), nullFeed)
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML.Light.Output (showTopElement)

import Web.Routes

import Language
import Models
import Routes
import Views

feedDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    Language -> [Article] -> m String
feedDisplay lang articles = do
    siteName <- getLangString (singleLanguage lang) "siteName"
    siteId <- gets appAddress
    let lastUpdated = arAuthored $ head articles
    let lastUpdatedStr = showGregorian (utctDay lastUpdated) ++ "T00:00:00Z"
    let feed = nullFeed siteId (TextString siteName) lastUpdatedStr
    return $ showTopElement $ xmlFeed feed
