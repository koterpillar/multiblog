{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Integration.TestArticle where

import Integration.Base

import Test.Framework

test_article = do
    article <- makeRequest $ simpleRequest "/2015/01/01/first-test"
    assertContains "<h1 id=\"first-test-article\">First test article</h1>" article
