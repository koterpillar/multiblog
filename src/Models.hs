{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Models where

import           Control.Monad
import           Control.Monad.Reader

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LB
import           Data.Default.Class
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Data.Time
import           Data.Yaml

import           GHC.Generics         (Generic)

import           Cache
import           Types.Content
import           Types.Language
import           Utils

newtype Analytics =
    Analytics
        { anaGoogle :: Maybe String
        }
    deriving (Generic, Show)

instance Default Analytics

instance FromJSON Analytics where
    parseJSON =
        A.withObject "Object expected" $ \v -> Analytics <$> v .:? "google"

data AppData =
    AppData
        { appDirectory   :: String
        , appAddress     :: Text
        , appRealAddress :: Bool
        , appArticles    :: [Article]
        , appMeta        :: [Meta]
        , appStrings     :: Map Text LanguageString
        , appLinks       :: [Link]
        , appAnalytics   :: Analytics
        }
    deriving (Generic, Show)

instance Default AppData where
    def =
        AppData
            { appDirectory = def
            , appAddress = mempty
            , appRealAddress = False
            , appArticles = def
            , appMeta = def
            , appStrings = def
            , appLinks = def
            , appAnalytics = def
            }

mkDate :: Integer -> Int -> Int -> UTCTime
mkDate y m d = atMidnight $ fromGregorian y m d

atMidnight :: Day -> UTCTime
atMidnight day = UTCTime day 0

askApp :: MonadReader AppData m => m AppData
askApp = ask

askFiltered :: MonadReader AppData m => (Article -> Bool) -> m [Article]
askFiltered articleFilter = asks $ filter articleFilter . appArticles

askOne :: (MonadReader AppData m, MonadPlus m) => (Article -> Bool) -> m Article
askOne articleFilter = onlyOne $ askFiltered articleFilter

askMeta :: (MonadReader AppData m, MonadPlus m) => Text -> m Meta
askMeta slug = onlyOne $ asks $ filter (bySlug slug) . appMeta

-- Find all languages used on the site
allLanguages :: AppData -> Set Language
allLanguages app = Set.union articleLangs metaLangs
  where
    articleLangs = allContentLangs $ appArticles app
    metaLangs = allContentLangs $ appMeta app
    allContentLangs :: HasContent a => [a] -> Set Language
    allContentLangs = Set.unions . map contentLangs
    contentLangs :: HasContent a => a -> Set Language
    contentLangs = Set.fromList . Map.keys . getContent

newtype AppCache =
    AppCache
        { appcachePdf :: Cache (Language, Text) LB.ByteString
        }

instance HasCache (Language, Text) LB.ByteString AppCache where
    getCache = appcachePdf
