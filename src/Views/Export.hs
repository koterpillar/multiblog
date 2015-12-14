{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Views.Export where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U

import System.Exit
import System.Process (proc)
import System.Process.ByteString.Lazy

import Text.Blaze.Renderer.Utf8 as Utf8Renderer
import Text.Hamlet

import Text.Pandoc hiding (Meta)

import Web.Routes

import Cache
import Language
import Models
import Routes
import Views


-- Export a meta into one of the supported formats
metaExport :: (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadState AppCache m, MonadIO m) =>
    PageFormat -> LanguagePreference -> Meta -> m LB.ByteString
-- Pandoc uses TeX to render PDFs, which requires a lot of packages for Unicode
-- support, etc. Use wkhtmltopdf instead
metaExport Pdf lang meta = pdfExport lang meta
metaExport format lang meta = do
    let content = langContent lang meta
    let writer = case format of
                    Docx -> writeDocx
                    Pdf -> error "PDF export handled separately"
                    Html -> error "HTML is not an export format"
    res <- liftIO $ writer def content
    return res

-- Export a PDF using wkhtmltopdf
pdfExport ::  (MonadRoute m, URL m ~ Sitemap, MonadReader AppData m, MonadState AppCache m, MonadIO m) =>
    LanguagePreference -> Meta -> m LB.ByteString
pdfExport lang meta = withCacheM (bestLanguage lang, mtSlug meta) $ do
    let content = writeHtml def $ langContent lang meta
    let title = langTitle lang meta
    html <- render $(hamletFile "templates/pdf-export.hamlet")
    let htmlString = Utf8Renderer.renderMarkup html
    -- TODO: Cache the result
    wkhtmltopdf htmlString

-- Convert an HTML file into PDF using wkhtmltopdf
wkhtmltopdf :: MonadIO m => LB.ByteString -> m LB.ByteString
wkhtmltopdf html = liftIO $ do
    (exitCode, pdf, err) <- readCreateProcessWithExitCode wkProc html
    case exitCode of
      ExitSuccess -> return pdf
      ExitFailure _ -> error $ U.toString $ LB.toStrict err
  -- wkhtmltopdf requires an X display
  where wkProc = proc "xvfb-run" $ ["-a"] ++ wkArgs
        wkArgs = [ "wkhtmltopdf"
                 , "--margin-top", "15mm"
                 , "--margin-bottom", "15mm"
                 , "-q"
                 , "-", "-"
                 ]
