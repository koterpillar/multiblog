{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestRSS where

import Control.Monad

import Text.Atom.Feed
import Text.Atom.Feed.Import (elementFeed)
import Text.Atom.Feed.Validate

import Text.XML.Light

import Test.Framework

import Integration.Base

atomEntry :: QName
atomEntry = QName "entry" Nothing Nothing

test_home = do
    rss <- makeRequest $ simpleRequest "/feed/en"
    let Just xml = parseXMLDoc rss
    -- Make sure every entry validates
    assertEqual [] $
        flattenT $ mkTree [] $ map validateEntry $ findChildren atomEntry xml
    -- Check feed contents
    let Just feed = elementFeed xml
    assertEqual "Test site" $ txtToString $ feedTitle feed
    -- TODO: Web.Routes generate a link without the trailing slash
    assertEqual (testAddress ++ "/") $ feedId feed
    assertEqual "2015-02-01T00:00:00Z" $ feedUpdated feed
    let entries = feedEntries feed
    assertEqual ["Another article", "First test article"] $
        map (txtToString . entryTitle) $ take 2 entries
    let entry1 = head entries
    assertEqual ["Author Name"] $ map personName $ entryAuthors entry1
    let Just (HTMLContent content) = entryContent entry1
    assertEqual "<p>This article should appear above the first one.</p>" $ content
