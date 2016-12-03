{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Models where

import Control.Monad
import Control.Monad.Reader

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import Data.Yaml

import GHC.Generics (Generic)

import Cache
import Types.Content
import Types.Language
import Types.Services
import Utils

data Analytics = Analytics
    { anaGoogle :: Maybe String
    } deriving (Generic, Show)

instance Default Analytics

instance FromJSON Analytics where
    parseJSON =
        A.withObject "Object expected" $ \v -> Analytics <$> v .:? "google"

data AppData = AppData
    { appDirectory :: String
    , appAddress :: String
    , appArticles :: [Article]
    , appMeta :: [Meta]
    , appStrings :: M.Map String LanguageString
    , appLinks :: [Link]
    , appAnalytics :: Analytics
    , appServices :: AppServices
    , appCrossPost :: AppCrossPost
    } deriving (Generic, Show)

instance Default AppData

mkDate :: Integer -> Int -> Int -> UTCTime
mkDate y m d = atMidnight $ fromGregorian y m d

atMidnight :: Day -> UTCTime
atMidnight day = UTCTime day 0

askApp
    :: MonadReader AppData m
    => m AppData
askApp = ask

askFiltered
    :: MonadReader AppData m
    => (Article -> Bool) -> m [Article]
askFiltered articleFilter = asks $ filter articleFilter . appArticles

askOne
    :: (MonadReader AppData m, MonadPlus m)
    => (Article -> Bool) -> m Article
askOne articleFilter = onlyOne $ askFiltered articleFilter

askMeta
    :: (MonadReader AppData m, MonadPlus m)
    => String -> m Meta
askMeta slug = onlyOne $ asks $ filter (bySlug slug) . appMeta

-- Find all languages used on the site
allLanguages :: AppData -> S.Set Language
allLanguages app = S.union articleLangs metaLangs
  where
    articleLangs = allContentLangs $ appArticles app
    metaLangs = allContentLangs $ appMeta app
    allContentLangs
        :: HasContent a
        => [a] -> S.Set Language
    allContentLangs = S.unions . map contentLangs
    contentLangs
        :: HasContent a
        => a -> S.Set Language
    contentLangs = S.fromList . M.keys . getContent

data AppCache = AppCache
    { appcachePdf :: Cache (Language, String) LB.ByteString
    }

instance HasCache (Language, String) LB.ByteString AppCache where
    getCache = appcachePdf
