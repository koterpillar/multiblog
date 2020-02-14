{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Views.Feed where

import           Control.Monad.Reader

import           Data.Default.Class

import           Data.Monoid

import           Data.Time

import qualified Data.Text                as Text
import qualified Data.Text.Lazy

import           Data.XML.Types

import           Happstack.Server

import           Text.Atom.Feed           (Date, Entry (..), EntryContent (..),
                                           Feed (..), Link (..), Person (..),
                                           TextContent (..), nullEntry,
                                           nullFeed, nullLink, nullPerson)
import           Text.Atom.Feed.Export    (xmlFeed)

import           Text.Blaze.Renderer.Text (renderMarkup)

import           Text.HTML.DOM            (parseLT)

import qualified Text.XML                 as C

import           Models
import           Routes
import           Types.Content
import           Types.Language
import           Views

newtype AtomFeed =
    AtomFeed
        { unAtomFeed :: Feed
        }

instance ToMessage AtomFeed where
    toContentType _ = "application/atom+xml"
    toMessage = C.renderLBS def . elementToDoc . xmlFeed . unAtomFeed

-- TODO: show time properly when it's parsed
atomDate :: UTCTime -> Date
atomDate = (<> "T00:00:00Z") . Text.pack . showGregorian . utctDay

articleEntry :: MonadReader AppData m => Language -> Article -> m Entry
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
            renderMarkup $
            runPandocPure' $ writeHtml $ stripTitle $ langContent lpref article
    return
        entry
            { entryContent =
                  Just $
                  HTMLContent $ documentRoot $ C.toXMLDocument $ parseLT content
            , entryLinks = [nullLink articleLink]
            , entryAuthors = [nullPerson {personName = authorName}]
            }

feedDisplay :: MonadReader AppData m => Language -> [Article] -> m Response
feedDisplay lang articles = do
    siteName <- askLangString (singleLanguage lang) "siteName"
    -- TODO: Web.Routes generate a link without the trailing slash
    home <- (<> "/") <$> linkTo Index
    let lastUpdated = arAuthored $ head articles
    let blankFeed = nullFeed home (TextString siteName) (atomDate lastUpdated)
    entries <- mapM (articleEntry lang) articles
    selfAddress <- linkTo $ Routes.Feed lang
    let selfLink = (nullLink selfAddress) {linkRel = Just $ Left "self"}
    let feed = blankFeed {feedEntries = entries, feedLinks = [selfLink]}
    pure $ toResponse $ AtomFeed feed

elementToDoc :: Element -> C.Document
elementToDoc el =
    either (error . show) id $
    C.fromXMLDocument $ Document (Prologue [] Nothing []) el []
