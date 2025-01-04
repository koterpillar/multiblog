module TestViews where

import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Pandoc                   hiding (Meta)

import           Routes
import           Types.Content
import           Views

import           Test.HUnit

newtype TestLink =
    TestLink Text

instance Linkable TestLink where
    link (TestLink dest) = MetaView dest Nothing

unit_linkedHeader :: IO ()
unit_linkedHeader = do
    let source =
            Text.unlines
                [ "Header"
                , "------"
                , ""
                , "Text content"
                , ""
                , "Other header"
                , "------------"
                ]
    let pandoc = runPandocPure' $ readMarkdown def source
    assertEqual
        ""
        (Text.intercalate
             "\n"
             [ "<h2><a href=\"http://test\">Header</a></h2>"
             , "<p>Text content</p>"
             , "<h2>Other header</h2>"
             ]) $
        Data.Text.Lazy.toStrict $
        renderHtml $
        runPandocPure' $ writeHtml $ linkedHeader "http://test" pandoc

unit_paginated :: IO ()
unit_paginated = do
    let pages = [1 .. 100] :: [Integer]
    assertEqual "" (Paginated Nothing [1, 2, 3] (Just 2)) (paginate 3 1 pages)
    assertEqual "" (Paginated (Just 1) [4, 5, 6] (Just 3)) (paginate 3 2 pages)
