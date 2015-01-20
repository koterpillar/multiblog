{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module TestViews where

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc hiding (Meta)

import Routes
import Views

import Test.Framework

data TestLink = TestLink

instance Linkable TestLink where
    link = const Index

test_linkedHeader = do
    let source = unlines $ [ "Header"
                           , "======"
                           , ""
                           , "Text content"
                           , ""
                           , "Other header"
                           , "============"
                           ]
    assertEqual
        "<h2><a href=\"/\">Header</a></h2><p>Text content</p><h2>Other header</h2>"
        $ renderHtml $ writeHtml def $ linkedHeader TestLink $ readMarkdown def source
