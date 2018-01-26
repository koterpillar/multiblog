{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestMeta where

import qualified Data.ByteString.Lazy as LB
import Data.LanguageCodes

import Integration.Base

import Test.Framework

test_meta :: IO ()
test_meta = do
    meta <- makeRequestText $ simpleRequest "/meta"
    assertTextContains "<h1>Test Meta</h1>" meta

test_meta_html :: IO ()
test_meta_html = do
    meta <- makeRequestText $ simpleRequest "/meta.html"
    assertTextContains "<h1>Test Meta</h1>" meta

test_meta_pdf :: IO ()
test_meta_pdf = do
    meta_pdf <- makeRequest $ simpleRequest "/meta.pdf"
    assertEqual "%PDF" (LB.take 4 meta_pdf)

test_meta_pdf_ru :: IO ()
test_meta_pdf_ru = do
    meta_pdf <- makeRequest $ withLang1 RU $ simpleRequest "/meta.pdf"
    assertEqual "%PDF" (LB.take 4 meta_pdf)

test_meta_docx :: IO ()
test_meta_docx = do
    meta_docx <- makeRequest $ simpleRequest "/meta.docx"
    assertEqual
        "PK" -- DOCX are ZIP files
        (LB.take 2 meta_docx)
