{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TestViewsExport where

import Data.Function
import Data.List

import Views.Export

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Test.Framework

test_fixupHtml = do
    let html =
            renderHtml $
            H.html $
            do H.head $ do H.title "Html title"
               H.body $
                   do H.h3 "Section 1"
                      H.p "Div 1, p 1"
                      text "   "
                      H.p "Div 1, p 2"
                      H.ul $
                          do H.li "Div 1, item 1"
                             H.li "Div 1, item 2"
                      H.p "Div 1, p 3"
                      H.h3 "Section 2"
                      H.p "Section 2, p 1"
                      H.p "Section 2, p 2"
                      H.h3 "Another header"
                      H.p "Section 3, p 1"
    let expected =
            renderHtml $
            H.html $
            do H.head $ do H.title "Html title"
               H.body $
                   do H.div ! A.class_ "grouped" $
                          do H.h3 "Section 1"
                             H.p "Div 1, p 1"
                             text "   "
                             H.p "Div 1, p 2"
                             H.ul $
                                 do H.li "Div 1, item 1"
                                    H.li "Div 1, item 2"
                             H.p "Div 1, p 3"
                      H.div ! A.class_ "grouped" $
                          do H.h3 "Section 2"
                             H.p "Section 2, p 1"
                             H.p "Section 2, p 2"
                      H.div ! A.class_ "grouped" $
                          do H.h3 "Another header"
                             H.p "Section 3, p 1"
    assertEqual expected (fixupHtml html)
