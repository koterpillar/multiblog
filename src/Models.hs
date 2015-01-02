module Models where

import Data.Time

import Text.Pandoc hiding (Meta, readers)

import Language

type LanguageContent = LanguageMap Pandoc

class HasSlug a where
    getSlug :: a -> String

-- TODO: lens
class HasContent a where
    getContent :: a -> LanguageContent
    modifyContent :: (LanguageContent -> LanguageContent) -> a -> a

-- TODO: Should Article and Meta actually be one type?
data Article = Article { arSlug     :: String
                       , arContent  :: LanguageContent
                       , arAuthored :: UTCTime
                       }
    deriving (Eq, Show)

data Meta = Meta { mtSlug    :: String
                 , mtContent :: LanguageContent
                 }
    deriving (Eq, Show)

instance HasContent Article where
    getContent = arContent
    modifyContent f a = a { arContent = f $ arContent a }

instance HasContent Meta where
    getContent = mtContent
    modifyContent f m = m { mtContent = f $ mtContent m }

instance HasSlug Article where
    getSlug = arSlug

instance HasSlug Meta where
    getSlug = mtSlug

data AppState = AppState { appArticles :: [Article]
                         , appMeta     :: [Meta]
                         }
    deriving (Eq, Show)

mkDate :: Integer -> Int -> Int -> UTCTime
mkDate y m d = atMidnight $ fromGregorian y m d

atMidnight :: Day -> UTCTime
atMidnight day = UTCTime day 0

byDate :: Day -> Article -> Bool
byDate d = (== d) . utctDay . arAuthored

byYear :: Integer -> Article -> Bool
byYear y a = y == y'
    where (y', _, _) = toGregorian $ utctDay $ arAuthored a

byYearMonth :: Integer -> Int -> Article -> Bool
byYearMonth y m a = y == y' && m == m'
    where (y', m', _) = toGregorian $ utctDay $ arAuthored a

bySlug :: HasSlug a => String -> a -> Bool
bySlug slug = (== slug) . getSlug

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a
