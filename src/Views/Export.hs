{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Views.Export where

import Control.Monad
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

import Language
import Models
import Routes
import Views


metaExport :: (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m, MonadIO m) =>
    PageFormat -> LanguagePreference -> Meta -> m LB.ByteString
metaExport Pdf lang meta = pdfExport lang meta
metaExport format lang meta = do
    let content = langContent lang meta
    let writer = case format of
                    Docx -> writeDocx
                    _ -> error "invalid export format"
    res <- liftIO $ writer def content
    return res

pdfExport ::  (MonadRoute m, URL m ~ Sitemap, MonadState AppState m, MonadPlus m, MonadIO m) =>
    LanguagePreference -> Meta -> m LB.ByteString
pdfExport lang meta = do
    let content = writeHtml def $ langContent lang meta
    let title =langTitle lang meta
    html <- render $(hamletFile "templates/pdf-export.hamlet")
    let htmlString = Utf8Renderer.renderMarkup html
    -- TODO: Cache the result
    wkhtmltopdf htmlString

wkhtmltopdf :: MonadIO m => LB.ByteString -> m LB.ByteString
wkhtmltopdf html = liftIO $ do
    (exitCode, pdf, err) <- readCreateProcessWithExitCode wkProc html
    case exitCode of
      ExitSuccess -> return pdf
      ExitFailure _ -> error $ U.toString $ LB.toStrict err
  where wkProc = proc "xvfb-run" ["-a", "wkhtmltopdf", "-q", "-", "-"]
