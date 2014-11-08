module Models where

import Data.Time


data Article = Article { arTitle :: String
                       , arContent :: Content
                       , arAuthored :: UTCTime
                       }

type Content = String

byDate :: Day -> Article -> Bool
byDate d = (== d) . utctDay . arAuthored

byYear :: Integer -> Article -> Bool
byYear y a = y == y'
    where (y', _, _) = toGregorian $ utctDay $ arAuthored a

byYearMonth :: Integer -> Int -> Article -> Bool
byYearMonth y m a = y == y' && m == m'
    where (y', m', _) = toGregorian $ utctDay $ arAuthored a

bySlug :: String -> Article -> Bool
bySlug slug = (== slug) . slugify . arTitle

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a

-- TODO
slugify :: String -> String
slugify = id
