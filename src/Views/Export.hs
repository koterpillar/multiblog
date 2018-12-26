{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Views.Export where

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy as LB

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding

import System.Exit
import System.Process (CreateProcess, proc)
import System.Process.ByteString.Lazy

import Text.Blaze.Renderer.Text as TextRenderer
import Text.HTML.TagSoup
import Text.Hamlet
import Text.StringLike

import Text.Pandoc hiding (Meta)

import Web.Routes

import Cache
import Models
import Render
import Routes
import Types.Content
import Types.Language
import Views

-- Export a meta into one of the supported formats
metaExport ::
       ( MonadRoute m
       , URL m ~ Sitemap
       , MonadReader AppData m
       , MonadState AppCache m
       , MonadIO m
       )
    => PageFormat
    -> LanguagePreference
    -> Meta
    -> m (Export LB.ByteString)
-- Pandoc uses TeX to render PDFs, which requires a lot of packages for Unicode
-- support, etc. Use wkhtmltopdf instead
metaExport Pdf lang meta = pdfExport lang meta
metaExport Html _ _ = error "HTML is not an export format"
metaExport Docx lang meta = do
    let content = langContent lang meta
    pure $
        inline docx (metaExportFileName Docx meta) $
        runPandocPure' $ writeDocx def content

metaExportFileName :: PageFormat -> Meta -> Text
metaExportFileName format meta = Text.intercalate "." [metaName, fileExtension format]
  where
    metaName = mtExportSlug meta
    fileExtension Docx = "docx"
    fileExtension Html = error "HTML is not an export format"
    fileExtension Pdf = "pdf"

-- Export a PDF using wkhtmltopdf
pdfExport ::
       (MonadRoute m, URL m ~ Sitemap, MonadState AppCache m, MonadIO m)
    => LanguagePreference
    -> Meta
    -> m (Export LB.ByteString)
pdfExport lang meta =
    fmap (inline pdf (metaExportFileName Docx meta)) $
    withCacheM (bestLanguage lang, mtSlug meta) $ do
        let content = runPandocPure' $ writeHtml $ langContent lang meta
        let title = langTitle lang meta
        html <- render $(hamletFile "templates/pdf-export.hamlet")
        let htmlText = TextRenderer.renderMarkup html
        let fixedHtmlText = fixupHtml htmlText
        wkhtmltopdf $ encodeUtf8 fixedHtmlText

-- Convert an HTML file into PDF using wkhtmltopdf
wkhtmltopdf :: MonadIO m => LB.ByteString -> m LB.ByteString
wkhtmltopdf html =
    liftIO $ do
        (exitCode, pdf, err) <- readCreateProcessWithExitCode wkhtmlProc html
        let pdf' = filterWkhtmlWarnings pdf
        case exitCode of
            ExitSuccess -> return pdf'
            ExitFailure _ -> error $ TL.unpack $ decodeUtf8 err

-- wkhtmltopdf, wrapped in xvfb-run as it requires an X display
wkhtmlProc :: CreateProcess
#ifdef darwin_HOST_OS
wkhtmlProc = proc (NE.head wkArgs) (NE.tail wkArgs)
#else
wkhtmlProc = proc "xvfb-run" $ "-a" : NE.toList wkArgs
#endif
  where
    wkArgs =
        "wkhtmltopdf" :|
        [ "--margin-top"
        , "15mm"
        , "--margin-bottom"
        , "15mm"
        , "--zoom"
        , "0.78125"
        , "--quiet"
        , "-"
        , "-"
        ]

filterWkhtmlWarnings :: LB.ByteString -> LB.ByteString
filterWkhtmlWarnings output
    | startsWithError = filterWkhtmlWarnings $ dropThisLine output
    | otherwise = output
  where
    startsWithError =
        LB.isPrefixOf "QSslSocket" output ||
        LB.isPrefixOf "libpng warning" output ||
        LB.isPrefixOf "Warning: Ignoring XDG_SESSION_TYPE=wayland" output
    dropThisLine :: LB.ByteString -> LB.ByteString
    dropThisLine = LB.dropWhile isNewline . LB.dropWhile (not . isNewline)
    isNewline 10 = True
    isNewline 13 = True
    isNewline _ = False

data FixupState
    = Start
    | Joining { fsLevel :: Int }
    | JoinEnd

-- wkhtmltopdf doesn't follow page-break-before: avoid. Help it by grouping the
-- headers together with elements that follow into a div each.
fixupHtml :: StringLike str => str -> str
fixupHtml = renderTags . go Start . parseTags
-- Run through the tags and wrap the headers with the following paragraphs
-- in '<div class="grouped">...</div>'
  where
    go :: StringLike str => FixupState -> [Tag str] -> [Tag str]
    -- End of input
    go _ [] = []
    -- Opening tag, emit joiner_start if it's a header element and start
    -- watching for the end of the run
    go Start (t@(TagOpen tag _):ts) =
        if isHeader tag
            then joiner_start : t : go (Joining 0) ts
            else t : go Start ts
    -- Closing tag, pass as is
    go Start (t@(TagClose _):ts) = t : go Start ts
    -- Opening another tag, ignore everything inside
    go (Joining lvl) (t@(TagOpen _ _):ts) = t : go (Joining $ lvl + 1) ts
    -- Closing tag, maybe this is the end of the run
    go (Joining 0) (t@(TagClose _):ts) = t : go JoinEnd ts
    -- Closing a nested tag
    go (Joining lvl) (t@(TagClose _):ts) = t : go (Joining $ lvl - 1) ts
    -- Opening another tag after a closing tag; maybe it's another header tag and
    -- thus the end of the run
    go JoinEnd (t@(TagOpen tag _):ts) =
        if isHeader tag
            then joiner_end : joiner_start : t : go (Joining 0) ts
            else t : go (Joining 0) ts
    -- Closing another tag after closing the tag that started the run; this is
    -- definitely the end of the run
    go JoinEnd (t@(TagClose _):ts) = joiner_end : t : go Start ts
    -- Other things are copied as they are
    go st (t:ts) = t : go st ts
    -- Whether the tag is a header that needs to group things after it
    isHeader :: StringLike str => str -> Bool
    isHeader = isHeader' . toString
      where
        isHeader' "h3" = True
        isHeader' _ = False
    -- Tag to output before a paragraph element run
    joiner_start :: StringLike str => Tag str
    joiner_start =
        TagOpen (fromString "div") [(fromString "class", fromString "grouped")]
    -- Tag to output after a paragraph element run
    joiner_end :: StringLike str => Tag str
    joiner_end = TagClose (fromString "div")
