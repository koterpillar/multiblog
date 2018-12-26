{-# LANGUAGE OverloadedStrings #-}

module Render where

import qualified Data.ByteString    as B

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy     as TL

import           Happstack.Server   hiding (ContentType)

type ContentType = Text

css :: ContentType
css = "text/css"

javaScript :: ContentType
javaScript = "application/javascript"

docx :: ContentType
docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"

pdf :: ContentType
pdf = "application/pdf"

withContentType :: ContentType -> Response -> Response
withContentType ct = setHeaderBS "Content-Type" (encodeUtf8 ct)

newtype Stylesheet =
    Stylesheet TL.Text

instance ToMessage Stylesheet where
    toResponse = withContentType css . toResponse

newtype JavaScript =
    JavaScript TL.Text

instance ToMessage JavaScript where
    toResponse = withContentType javaScript . toResponse

data Export content = Export
    { exContent       :: content
    , exContentType   :: ContentType
    , exFileName      :: Text
    , exForceDownload :: Bool
    }

inline :: ContentType -> Text -> content -> Export content
inline contentType fileName content =
    Export
        { exContent = content
        , exContentType = contentType
        , exFileName = fileName
        , exForceDownload = False
        }

instance ToMessage content => ToMessage (Export content) where
    toResponse export =
        setHeaderBS "Content-Disposition" (encodeUtf8 disposition) $
        withContentType (exContentType export) $ toResponse (exContent export)
      where
        disposition =
            Text.intercalate "; "
                [ if exForceDownload export
                      then "attachment"
                      else "inline"
                , "filename=\"" <> exFileName export <> "\""
                ]
