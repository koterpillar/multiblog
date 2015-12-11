{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestMeta where

import Control.Monad

import Data.LanguageCodes

import Language

import Integration.Base

import Test.Framework


test_meta = do
    meta <- makeRequest $ simpleRequest "/meta"
    assertContains
        "<h1 id=\"test-meta\">Test Meta</h1>"
        meta

test_meta_html = do
    meta <- makeRequest $ simpleRequest "/meta.html"
    assertContains
        "<h1 id=\"test-meta\">Test Meta</h1>"
        meta

test_meta_pdf = do
    meta_pdf <- makeRequest $ simpleRequest "/meta.pdf"
    assertEqual
        "%PDF"
        (take 4 meta_pdf)

test_meta_docx = do
    meta_docx <- makeRequest $ simpleRequest "/meta.docx"
    assertEqual
        "PK"  -- DOCX are ZIP files
        (take 2 meta_docx)
