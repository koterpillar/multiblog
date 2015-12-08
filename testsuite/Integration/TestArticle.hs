{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestArticle where

import Control.Monad

import Data.LanguageCodes

import Language

import Integration.Base

import Test.Framework


test_article = do
    article <- makeRequest $ simpleRequest "/2015/01/01/first-test"
    assertContains
        "<h2 id=\"first-test-article\">First test article</h2>"
        article
