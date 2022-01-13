{-# LANGUAGE OverloadedStrings #-}

module Integration.TestMeta where

import qualified Data.ByteString.Lazy as LB
import           Data.Foldable
import           Data.LanguageCodes
import           Data.Text.Encoding   (decodeUtf8)

import           Integration.Base
import           Test.HUnit

unit_meta :: IO ()
unit_meta = do
    meta <- makeRequestText $ simpleRequest "/meta"
    meta `shouldContainText` "<h1>Test Meta</h1>"

unit_meta_pdf :: IO ()
unit_meta_pdf = do
    meta_pdf <- makeRequest $ simpleRequest "/meta.pdf"
    meta_pdf_content <- responseContent meta_pdf
    assertEqual "" "%PDF" (LB.take 4 meta_pdf_content)
    assertEqual
        ""
        (Just "inline; filename=\"meta.pdf\"")
        (responseHeader "Content-Disposition" meta_pdf)

unit_meta_export_custom_slug :: IO ()
unit_meta_export_custom_slug =
    for_ ["pdf", "docx"] $ \format -> do
        meta_pdf <- makeRequest $ simpleRequest $ "/custom-slug." <> format
        assertEqual
            ""
            (Just $ "inline; filename=\"customized-slug." <> format <> "\"")
            (decodeUtf8 <$> responseHeader "Content-Disposition" meta_pdf)

unit_meta_pdf_ru :: IO ()
unit_meta_pdf_ru = do
    meta_pdf <- makeRequest $ withLang1 RU $ simpleRequest "/meta.pdf"
    meta_pdf_content <- responseContent meta_pdf
    assertEqual "" "%PDF" (LB.take 4 meta_pdf_content)

unit_meta_docx :: IO ()
unit_meta_docx = do
    meta_docx <- makeRequest $ simpleRequest "/meta.docx"
    meta_docx_content <- responseContent meta_docx
    assertEqual
        ""
        "PK" -- DOCX are ZIP files
        (LB.take 2 meta_docx_content)
    assertEqual
        ""
        (Just "inline; filename=\"meta.docx\"")
        (responseHeader "Content-Disposition" meta_docx)
