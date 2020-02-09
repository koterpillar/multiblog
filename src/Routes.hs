{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Prelude hiding ((.))

import Control.Category (Category((.)))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import GHC.Generics

import Types.Language

data PageFormat
    = Pdf
    | Docx
    deriving (Eq, Ord, Show, Generic)

formatToStr :: PageFormat -> Text
formatToStr Pdf = "pdf"
formatToStr Docx = "docx"

strToFormat :: Text -> Maybe PageFormat
strToFormat "pdf" = Just Pdf
strToFormat "docx" = Just Docx
strToFormat _ = Nothing

type MaybeFormat = Maybe PageFormat

data Sitemap
    = Index
    | ArticleView Day
                  Text
    | MetaView Text
               MaybeFormat
    | Feed Language
    | SiteScript
    | PrintStylesheet
    | CodeStylesheet
    deriving (Eq, Ord, Show, Generic)

routeURL :: Sitemap -> Text
routeURL = undefined

parseURL :: Text -> Maybe Sitemap
parseURL = undefined

-- Join a basename and extension together
joinExt :: Text -> Maybe Text -> Text
joinExt segment Nothing = segment
joinExt segment (Just ext) = segment <> "." <> ext
