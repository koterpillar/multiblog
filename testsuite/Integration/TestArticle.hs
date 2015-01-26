{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestArticle where

import Control.Monad

import Data.LanguageCodes

import Language

import Integration.Base

import Test.Framework


test_article = do
    req <- mkRequest "/2015/01/01/first-test"
    article <- testRequest req
    assertContains
        "<h2 id=\"first-test-article\">First test article</h2>"
        $ responseContent article
