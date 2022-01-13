{-# LANGUAGE OverloadedStrings #-}

module TestViewsExport where

import           Views.Export

import           Text.Blaze
import           Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

test_fixupHtml :: IO ()
test_fixupHtml = do
    let section1 = do
            H.h3 "Section 1"
            H.p "Div 1, p 1"
            text "   "
            H.p "Div 1, p 2"
            H.ul $ do
                H.li "Div 1, item 1"
                H.li "Div 1, item 2"
            H.p "Div 1, p 3"
    let section2 = do
            H.h3 "Section 2"
            H.p "Section 2, p 1"
            H.p "Section 2, p 2"
    let section3 = do
            H.h3 "Another header"
            H.p "Section 3, p 1"
    let html =
            renderHtml $
            H.html $ do
                H.head $ H.title "Html title"
                H.body $ do
                    section1
                    section2
                    section3
    let expected =
            renderHtml $
            H.html $ do
                H.head $ H.title "Html title"
                H.body $ do
                    H.div ! A.class_ "grouped" $ section1
                    H.div ! A.class_ "grouped" $ section2
                    H.div ! A.class_ "grouped" $ section3
    assertEqual expected (fixupHtml html)
