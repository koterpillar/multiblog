{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestBase where

import Happstack.Server

import Test.Framework

import App
import Import

testRequest :: Request -> IO Response
testRequest req = do
    Right app <- loadFromDirectory "testsuite/Integration/test_content"
    runApp app (simpleHTTP'' (siteHandler "http://test") req)

test_whatever = do
    assertEqual "" ""
