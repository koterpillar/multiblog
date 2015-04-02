{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

import Text.Pandoc hiding (Meta, readers)

import Language
import Utils

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

instance Ord Article where
    a `compare` b = (arAuthored a, arSlug a) `compare` (arAuthored b, arSlug b)

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

data AppState = AppState { appDirectory :: String
                         , appAddress   :: String
                         , appArticles  :: [Article]
                         , appMeta      :: [Meta]
                         , appStrings   :: M.Map String (LanguageMap String)
                         }
    deriving (Eq, Show)

emptyState :: AppState
emptyState = AppState { appDirectory = ""
                      , appAddress = ""
                      , appArticles = []
                      , appMeta = []
                      , appStrings = M.empty
                      }

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

getApp :: MonadState AppState m => m AppState
getApp = get

getFiltered :: MonadState AppState m => (Article -> Bool) -> m [Article]
getFiltered articleFilter = gets $ filter articleFilter . appArticles

getOne :: (MonadState AppState m, MonadPlus m) =>
    (Article -> Bool) -> m Article
getOne articleFilter = onlyOne $ getFiltered articleFilter

getMeta :: (MonadState AppState m, MonadPlus m) => String -> m Meta
getMeta slug = onlyOne $ gets $ filter (bySlug slug) . appMeta

-- Find all languages used on the site
allLanguages :: AppState -> S.Set Language
allLanguages app = S.union articleLangs metaLangs
    where articleLangs = allContentLangs $ appArticles app
          metaLangs = allContentLangs $ appMeta app
          allContentLangs :: HasContent a => [a] -> S.Set Language
          allContentLangs = S.unions . map contentLangs
          contentLangs :: HasContent a => a -> S.Set Language
          contentLangs = S.fromList . M.keys . getContent
