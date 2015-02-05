{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestRSS where

import Control.Monad

import Text.Atom.Feed
import Text.Atom.Feed.Import (elementFeed)
import Text.XML.Light.Input (parseXMLDoc)

import Test.Framework

import Integration.Base


test_home = do
    req <- mkRequest "/feed/en"
    rss <- testResponse req
    print rss
    let Just xml = parseXMLDoc rss
    let Just feed = elementFeed xml
    assertEqual "Test site" $ txtToString $ feedTitle feed
    assertEqual testAddress $ feedId feed
