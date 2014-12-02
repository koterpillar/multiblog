module Models where

import qualified Data.Map as M
import Data.Time

import Text.Pandoc hiding (readers)

import Language

data Article = Article { arSlug     :: String
                       , arContent  :: M.Map Language Pandoc
                       , arAuthored :: UTCTime
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

bySlug :: String -> Article -> Bool
bySlug slug = (== slug) . arSlug

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a
