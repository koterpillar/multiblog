module Routes where

import           Prelude          hiding ((.))

import           Control.Category (Category ((.)))

import           Data.String.Here (i)

import           Data.Char        (isDigit)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Time

import           GHC.Generics

import           Text.Read

import           Types.Language

data PageFormat
    = Pdf
    | Docx
    deriving (Eq, Ord, Show, Generic)

formatToStr :: PageFormat -> Text
formatToStr Pdf  = "pdf"
formatToStr Docx = "docx"

strToFormat :: Text -> Maybe PageFormat
strToFormat "pdf"  = Just Pdf
strToFormat "docx" = Just Docx
strToFormat _      = Nothing

type MaybeFormat = Maybe PageFormat

data Sitemap
    = Index
    | ArticleView Day Text
    | MetaView Text MaybeFormat
    | Feed Language
    | SiteScript
    | PrintStylesheet
    | CodeStylesheet
    deriving (Eq, Ord, Show, Generic)

routeURL :: Sitemap -> Text
routeURL Index = ""
routeURL (ArticleView date text) =
    let (year, month, day) = toGregorian date
     in [i|/${pad 2 year}-${pad 2 month}-${pad 2 day}/${text}|]
routeURL (MetaView text Nothing) = [i|/${text}|]
routeURL (MetaView text (Just format)) = [i|/${text}.${formatToStr format}|]
routeURL (Feed language) = [i|/feed/${showLanguage language}|]
routeURL SiteScript = "/assets/site.js"
routeURL PrintStylesheet = "/assets/print.css"
routeURL CodeStylesheet = "/assets/code.css"

pad :: Show a => Int -> a -> Text
pad len str =
    let printed = Text.pack (show str)
        padding = Text.replicate (len - Text.length printed) "0"
     in padding <> printed

splitNonEmpty :: Text -> Text -> [Text]
splitNonEmpty splitter = filter (not . Text.null) . Text.splitOn splitter

readText :: Read a => Text -> Maybe a
readText = readMaybe . Text.unpack

parseURL :: Text -> Maybe Sitemap
parseURL = parseSegments . splitNonEmpty "/" . dropQueryParams
  where
    parseSegments [] = Just Index
    parseSegments [metaText] =
        case splitNonEmpty "." metaText of
            [text] -> Just $ MetaView text Nothing
            [text, formatStr] -> do
                format <- strToFormat formatStr
                pure $ MetaView text (Just format)
            _ -> Nothing
    parseSegments [date, articleText]
        | dateLike date = parseArticle (splitNonEmpty "-" date) articleText
    parseSegments [year, month, day, articleText] =
        parseArticle [year, month, day] articleText
    parseSegments ["assets", "site.js"] = Just SiteScript
    parseSegments ["assets", "print.css"] = Just PrintStylesheet
    parseSegments ["assets", "code.css"] = Just CodeStylesheet
    parseSegments ["feed", lang] = Feed <$> parseLanguageM lang
    parseSegments _ = Nothing
    dateLike = Text.all $ \c -> isDigit c || c == '-'
    parseArticle [yearStr, monthStr, dayStr] text = do
        year <- readText yearStr
        month <- readText monthStr
        day <- readText dayStr
        date <- fromGregorianValid year month day
        pure $ ArticleView date text
    parseArticle _ _ = Nothing
    dropQueryParams = Text.takeWhile (/= '?')

-- Join a basename and extension together
joinExt :: Text -> Maybe Text -> Text
joinExt segment Nothing    = segment
joinExt segment (Just ext) = segment <> "." <> ext
