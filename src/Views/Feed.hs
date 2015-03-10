{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Views.Feed where

import Control.Monad.State

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U
import Data.Time

import Happstack.Server

import Text.Atom.Feed (Date, Entry (..), EntryContent (..), Feed (..),
                       TextContent (..), nullEntry, nullFeed, nullLink)
import Text.Atom.Feed.Export (xmlFeed)

import Text.Blaze.Renderer.String (renderMarkup)

import Text.Pandoc (def, writeHtml)

import Text.XML.Light (Element)
import Text.XML.Light.Output (showTopElement)

import Web.Routes

import Language
import Models
import Routes
import Views

data AtomFeed = AtomFeed { unAtomFeed :: Element }

instance ToMessage AtomFeed where
    toContentType _ = "application/atom+xml"
    toMessage = LB.fromStrict . U.fromString . showTopElement . unAtomFeed

-- TODO: show time properly when it's parsed
atomDate :: UTCTime -> Date
atomDate = (++ "T00:00:00Z") . showGregorian . utctDay

articleEntry :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m) => Language -> Article -> m Entry
articleEntry lang article = do
    let lpref = singleLanguage lang
    articleLink <- linkTo article
    let entry = nullEntry articleLink (TextString $ langTitle lpref article) (atomDate $ arAuthored article)
    let content = renderMarkup $ writeHtml def $ langContent lpref article
    return entry { entryContent = Just $ HTMLContent content
                 , entryLinks = [nullLink articleLink]
                 }

feedDisplay :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m) =>
    Language -> [Article] -> m Response
feedDisplay lang articles = do
    siteName <- getLangString (singleLanguage lang) "siteName"
    home <- linkTo Index
    -- TODO: Web.Routes generate a link without the trailing slash
    let siteId = home ++ "/"
    let lastUpdated = arAuthored $ head articles
    let blankFeed = nullFeed siteId (TextString siteName) (atomDate lastUpdated)
    entries <- mapM (articleEntry lang) articles
    let feed = blankFeed { feedEntries = entries }
    return $ toResponse $ AtomFeed $ xmlFeed feed
