{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestArticle where

import           Integration.Base

unit_article = do
    article <- makeRequestText $ simpleRequest "/2015/01/01/first-test"
    article `shouldContainText` "<h1>First test article</h1>"

unit_article_with_code = do
    article <- makeRequestText $ simpleRequest "/2018/01/01/some-code"
    article `shouldContainText` "<code"
