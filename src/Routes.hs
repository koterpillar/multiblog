{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Prelude hiding ((.))

import Control.Category (Category((.)))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Text.Boomerang.TH (makeBoomerangs)

import Web.Routes.Boomerang

import Types.Language

data PageFormat
    = Html
    | Pdf
    | Docx
    deriving (Eq, Ord, Show)

-- TODO: use Boomerang for these
formatToStr :: PageFormat -> Text
formatToStr Html = "pdf"
formatToStr Pdf = "pdf"
formatToStr Docx = "docx"

strToFormat :: Text -> Maybe PageFormat
strToFormat "html" = Just Html
strToFormat "pdf" = Just Pdf
strToFormat "docx" = Just Docx
strToFormat _ = Nothing

type MaybeFormat = Maybe PageFormat

data Sitemap
    = Index
    | Yearly Integer
    | Monthly Integer
              Int
    | Daily Day
    | ArticleView Day
                  Text
    | MetaView Text
               MaybeFormat
    | Feed Language
    | SiteScript
    | PrintStylesheet
    deriving (Eq, Ord, Show)

makeBoomerangs ''Sitemap

rDay :: Boomerang TextsError [Text] r (Day :- r)
rDay = xpure mkDay parseDay . (integer </> int </> int)
  where
    mkDay (y :- m :- d :- x) = fromGregorian y m d :- x
    parseDay (day :- x) =
        let (y, m, d) = toGregorian day
        in Just $ y :- m :- d :- x

-- TODO: This will error on strings which are not language codes
rLanguage :: Boomerang TextsError [Text] r (Language :- r)
rLanguage = xpure mkLang parseLang . anyString
  where
    mkLang (str :- x) =
        let Just lang = parseLanguageM str
        in lang :- x
    parseLang (lang :- x) = Just $ showLanguage lang :- x

rString :: Boomerang e tok i (Text :- o) -> Boomerang e tok i (String :- o)
rString = xmaph T.unpack (Just . T.pack)

anyString :: Boomerang TextsError [Text] o (String :- o)
anyString = rString anyText

rExtension
    :: Boomerang e tok i (Text :- o)
    -> Boomerang e tok i (Text :- Maybe Text :- o)
rExtension = xmap splitExt' (Just . joinExt')
  where
    splitExt' :: Text :- o -> Text :- Maybe Text :- o
    splitExt' (seg :- o) =
        let (seg', ext) = splitExt seg
        in seg' :- ext :- o
    joinExt' :: Text :- Maybe Text :- o -> Text :- o
    joinExt' (seg :- ext :- o) = joinExt seg ext :- o

-- Swap the top 2 components in a Boomerang
xflip :: Boomerang e tok i (a :- b :- o) -> Boomerang e tok i (b :- a :- o)
xflip = xmap pflip (Just . pflip)
  where
    pflip (a :- b :- o) = b :- a :- o

-- Apply a transformation to the second topmost component of a Boomerang
xmaph2
    :: (b -> c)
    -> (c -> Maybe b)
    -> Boomerang e tok i (a :- b :- o)
    -> Boomerang e tok i (a :- c :- o)
xmaph2 f g = xflip . xmaph f g . xflip

-- Split a path component into basename and extension
splitExt :: Text -> (Text, Maybe Text)
splitExt segment =
    case T.splitOn "." segment of
        [] -> ("", Nothing)
        [s] -> (s, Nothing)
        ss -> (T.intercalate "." $ init ss, Just $ last ss)

-- Join a basename and extension together
joinExt :: Text -> Maybe Text -> Text
joinExt segment Nothing = segment
joinExt segment (Just ext) = segment <> "." <> ext

-- Convert the second topmost component into a MaybeFormat
xFormat
    :: Boomerang e tok i (Text :- Maybe Text :- o)
    -> Boomerang e tok i (Text :- MaybeFormat :- o)
xFormat = xmaph2 (strToFormat =<<) (Just . fmap formatToStr)

sitemap :: Boomerang TextsError [Text] r (Sitemap :- r)
sitemap =
    mconcat
        [ rIndex
        , rYearly . integer
        , rMonthly . integer </> int
        , rDaily . rDay
        , rFeed . "feed" </> rLanguage
        , rSiteScript . "site.js"
        , rPrintStylesheet . "print.css"
        , rArticleView . rDay </> anyText
        , rMetaView . xFormat (rExtension anyText)
        ]
