{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module TestViews where

import Data.List

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc hiding (Meta)

import Routes
import Views

import Test.Framework

data TestLink =
    TestLink String

instance Linkable TestLink where
    link (TestLink dest) = MetaView dest Nothing

test_linkedHeader = do
    let source =
            unlines
                [ "Header"
                , "------"
                , ""
                , "Text content"
                , ""
                , "Other header"
                , "------------"
                ]
    let (Right pandoc) = readMarkdown def source
    assertEqual
        (intercalate
             "\n"
             [ "<h2><a href=\"http://test\">Header</a></h2>"
             , "<p>Text content</p>"
             , "<h2>Other header</h2>"
             ]) $
        renderHtml $ writeHtml def $ linkedHeader "http://test" pandoc

test_paginated = do
    let pages = [1..100] :: [Integer]
    assertEqual
      (Paginated Nothing [1, 2, 3] (Just 2))
      (paginate 3 1 pages)
    assertEqual
      (Paginated (Just 1) [4, 5, 6] (Just 3))
      (paginate 3 2 pages)
