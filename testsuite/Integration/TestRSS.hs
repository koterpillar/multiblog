{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestRSS where

import Data.Default.Class
import Data.Monoid

import Data.XML.Types

import Text.Atom.Feed
import Text.Atom.Feed.Import (elementFeed)
import Text.Atom.Feed.Validate

import qualified Text.XML as C

import Test.Framework

import Integration.Base

atomEntry :: Name
atomEntry = "entry"

test_home = do
    rss <- makeRequest $ simpleRequest "/feed/en"
    xml <-
        case C.parseLBS def rss of
            Left exc -> error $ show exc
            Right res -> pure res
    let root = documentRoot $ C.toXMLDocument xml
    -- Make sure every entry validates
    -- let entries = findChildren atomEntry root
    let entryElements = elementChildren root
    assertNotEmpty entryElements
    assertEqual [] $ flattenT $ mkTree [] $ map validateEntry entryElements
    -- Check feed contents
    let Just feed = elementFeed root
    assertEqual "Test site" $ txtToString $ feedTitle feed
    -- TODO: Web.Routes generate a link without the trailing slash
    assertEqual (testAddress <> "/") $ feedId feed
    assertEqual "2015-02-01T00:00:00Z" $ feedUpdated feed
    let entries = feedEntries feed
    assertEqual ["Another article", "First test article"] $
        map (txtToString . entryTitle) $ take 2 entries
    let entry1 = head entries
    assertEqual ["Author Name"] $ map personName $ entryAuthors entry1
    let Just (HTMLContent content) = entryContent entry1
    assertEqual "<p>This article should appear above the first one.</p>" content
