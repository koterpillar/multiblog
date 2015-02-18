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
    req <- mkRequest "/feed/en"
    rss <- testResponse req
    let Just xml = parseXMLDoc rss
    -- Make sure every entry validates
    assertEqual [] $ flattenT $ mkTree [] $ map validateEntry $ findChildren atomEntry xml
    -- Check feed contents
    let Just feed = elementFeed xml
    assertEqual "Test site" $ txtToString $ feedTitle feed
    assertEqual testAddress $ feedId feed
    assertEqual "2015-02-01T00:00:00Z" $ feedUpdated feed
    assertEqual [ "Another article"
                , "First test article"
                ]
        $ map (txtToString . entryTitle) $ feedEntries feed
