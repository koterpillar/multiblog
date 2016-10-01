{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Views.Feed where

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U
import Data.Time

import Happstack.Server

import Text.Atom.Feed
       (Date, Entry(..), EntryContent(..), Feed(..), Link(..), Person(..),
        TextContent(..), nullEntry, nullFeed, nullLink, nullPerson)
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

data AtomFeed = AtomFeed
    { unAtomFeed :: Element
    }

instance ToMessage AtomFeed where
    toContentType _ = "application/atom+xml"
    toMessage = LB.fromStrict . U.fromString . showTopElement . unAtomFeed

-- TODO: show time properly when it's parsed
atomDate :: UTCTime -> Date
atomDate = (++ "T00:00:00Z") . showGregorian . utctDay

articleEntry
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m)
    => Language -> Article -> m Entry
articleEntry lang article = do
    let lpref = singleLanguage lang
    articleLink <- linkTo article
    authorName <- askLangString lpref "authorName"
    let entry =
            nullEntry
                articleLink
                (TextString $ langTitle lpref article)
                (atomDate $ arAuthored article)
    let content =
            renderMarkup $ writeHtml def $ stripTitle $ langContent lpref article
    return
        entry
        { entryContent = Just $ HTMLContent content
        , entryLinks = [nullLink articleLink]
        , entryAuthors =
            [ nullPerson
              { personName = authorName
              }
            ]
        }

feedDisplay
    :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m)
    => Language -> [Article] -> m Response
feedDisplay lang articles = do
    siteName <- askLangString (singleLanguage lang) "siteName"
    -- TODO: Web.Routes generate a link without the trailing slash
    home <- liftM (++ "/") $ linkTo Index
    let lastUpdated = arAuthored $ head articles
    let blankFeed = nullFeed home (TextString siteName) (atomDate lastUpdated)
    entries <- mapM (articleEntry lang) articles
    selfAddress <- linkTo $ Routes.Feed lang
    let selfLink =
            (nullLink selfAddress)
            { linkRel = Just $ Left "self"
            }
    let feed =
            blankFeed
            { feedEntries = entries
            , feedLinks = [selfLink]
            }
    return $ toResponse $ AtomFeed $ xmlFeed feed
