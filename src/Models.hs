{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Models where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Time

import Text.Pandoc hiding (Meta, readers)
import Text.Pandoc.Walk

import Cache
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

data AppData = AppData { appDirectory :: String
                       , appAddress   :: String
                       , appArticles  :: [Article]
                       , appMeta      :: [Meta]
                       , appStrings   :: M.Map String (LanguageMap String)
                       }
    deriving (Eq, Show)

emptyState :: AppData
emptyState = AppData { appDirectory = ""
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

askApp :: MonadReader AppData m => m AppData
askApp = ask

askFiltered :: MonadReader AppData m => (Article -> Bool) -> m [Article]
askFiltered articleFilter = asks $ filter articleFilter . appArticles

askOne :: (MonadReader AppData m, MonadPlus m) =>
    (Article -> Bool) -> m Article
askOne articleFilter = onlyOne $ askFiltered articleFilter

askMeta :: (MonadReader AppData m, MonadPlus m) => String -> m Meta
askMeta slug = onlyOne $ asks $ filter (bySlug slug) . appMeta

-- Find all languages used on the site
allLanguages :: AppData -> S.Set Language
allLanguages app = S.union articleLangs metaLangs
    where articleLangs = allContentLangs $ appArticles app
          metaLangs = allContentLangs $ appMeta app
          allContentLangs :: HasContent a => [a] -> S.Set Language
          allContentLangs = S.unions . map contentLangs
          contentLangs :: HasContent a => a -> S.Set Language
          contentLangs = S.fromList . M.keys . getContent

langTitle :: HasContent a => LanguagePreference -> a -> String
langTitle lang = fromMaybe "untitled" . listToMaybe . query extractTitle . langContent lang
    where extractTitle (Header _ _ title) = [inlineToStr title]
          extractTitle _ = []

-- TODO: Might be a better way to do this in Pandoc
inlineToStr :: [Inline] -> String
inlineToStr inline = writePlain def $ Pandoc undefined [Plain inline]

stripTitle :: Pandoc -> Pandoc
stripTitle (Pandoc meta blocks) = Pandoc meta blocks'
    where blocks' = catMaybes $ evalState (mapM stripFirst blocks) True
          stripFirst :: Block -> State Bool (Maybe Block)
          stripFirst block = do
              isFirst <- get
              if isFirst
                  then case block of
                      Header _ _ _ -> do
                          put False
                          return Nothing
                      _ -> return $ Just block
                  else return $ Just block

langContent :: HasContent a => LanguagePreference -> a -> Pandoc
langContent lang = fromJust . matchLanguage lang . getContent

data AppCache = AppCache { appcachePdf :: Cache (Language, String) LB.ByteString
                         }

instance HasCache (Language, String) LB.ByteString AppCache where
    getCache = appcachePdf
