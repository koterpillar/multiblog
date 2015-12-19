{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Routes where

import Prelude hiding ((.))

import Control.Category (Category ((.)))

import Data.List
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Text.Boomerang.TH (makeBoomerangs)

import Web.Routes.Boomerang

import Language

data PageFormat = Html | Pdf | Docx
    deriving (Eq, Show)

-- TODO: use Boomerang for these
formatToStr :: PageFormat -> String
formatToStr Html = "pdf"
formatToStr Pdf = "pdf"
formatToStr Docx = "docx"

strToFormat :: String -> Maybe PageFormat
strToFormat "html" = Just Html
strToFormat "pdf" = Just Pdf
strToFormat "docx" = Just Docx
strToFormat _ = Nothing

type MaybeFormat = Maybe PageFormat

data Sitemap = Index
           | Yearly Integer
           | Monthly Integer Int
           | Daily Day
           | ArticleView Day String
           | MetaView String MaybeFormat
           | Feed Language
           | SiteScript
           | PrintStylesheet
           deriving (Eq, Show)

makeBoomerangs ''Sitemap

rDay :: Boomerang TextsError [Text] r (Day :- r)
rDay = xpure mkDay parseDay . (integer </> int </> int)
    where mkDay (y :- m :- d :- x) = fromGregorian y m d :- x
          parseDay (day :- x) = let (y, m, d) = toGregorian day in Just $ y :- m :- d :- x

-- TODO: This will error on strings which are not language codes
rLanguage :: Boomerang TextsError [Text] r (Language :- r)
rLanguage = xpure mkLang parseLang . anyString
    where mkLang (str :- x) = let Just lang = parseLanguage str in lang :- x
          parseLang (lang :- x) = Just $ showLanguage lang :- x

rString :: Boomerang e tok i (Text :- o) -> Boomerang e tok i (String :- o)
rString = xmaph T.unpack (Just . T.pack)

anyString :: Boomerang TextsError [Text] o (String :- o)
anyString = rString anyText

rExtension :: Boomerang e tok i (String :- o) -> Boomerang e tok i (String :- Maybe String :- o)
rExtension = xmap splitExt' (Just . joinExt')
    where splitExt' :: String :- o -> String :- Maybe String :- o
          splitExt' (seg :- o) = let (seg', ext) = splitExt seg in seg' :- ext :- o
          joinExt' :: String :- Maybe String :- o -> String :- o
          joinExt' (seg :- ext :- o) = joinExt seg ext :- o

-- Swap the top 2 components in a Boomerang
xflip :: Boomerang e tok i (a :- b :- o) -> Boomerang e tok i (b :- a :- o)
xflip = xmap pflip (Just . pflip)
    where pflip (a :- b :- o) = b :- a :- o

-- Apply a transformation to the second topmost component of a Boomerang
xmaph2 :: (b -> c) -> (c -> Maybe b) -> Boomerang e tok i (a :- b :- o) -> Boomerang e tok i (a :- c :- o)
xmaph2 f g = xflip . xmaph f g . xflip

-- Split a path component into basename and extension
splitExt :: String -> (String, Maybe String)
splitExt segment = case splitOn "." segment of
                     [] -> ("", Nothing)
                     [s] -> (s, Nothing)
                     ss -> (intercalate "." $ init ss, Just $ last ss)

-- Join a basename and extension together
joinExt :: String -> Maybe String -> String
joinExt segment Nothing = segment
joinExt segment (Just ext) = segment ++ "." ++ ext

-- Convert the second topmost component into a MaybeFormat
xFormat :: Boomerang e tok i (String :- Maybe String :- o) -> Boomerang e tok i (String :- MaybeFormat :- o)
xFormat = xmaph2 ((=<<) strToFormat) (Just . fmap formatToStr)

sitemap :: Boomerang TextsError [Text] r (Sitemap :- r)
sitemap = mconcat
    [ rIndex
    , rYearly . integer
    , rMonthly . integer </> int
    , rDaily . rDay
    , rFeed . "feed" </> rLanguage
    , rSiteScript . "site.js"
    , rPrintStylesheet . "print.css"
    , rArticleView . rDay </> anyString
    , rMetaView . xFormat (rExtension anyString)
    ]
