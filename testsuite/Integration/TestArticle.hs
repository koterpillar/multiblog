{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestArticle where

import           Integration.Base

import           Test.Framework

test_article = do
    article <- makeRequestText $ simpleRequest "/2015/01/01/first-test"
    assertTextContains "<h1>First test article</h1>" article

test_article_with_code = do
    article <- makeRequestText $ simpleRequest "/2018/01/01/some-code"
    assertTextContains "<code" article
