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
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           Data.Yaml

import           GHC.Generics         (Generic)

import           Cache
import           Types.Content
import           Types.Language
import           Types.Services
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

data AppData = AppData
    { appDirectory   :: String
    , appAddress     :: T.Text
    , appRealAddress :: Bool
    , appArticles    :: [Article]
    , appMeta        :: [Meta]
    , appStrings     :: M.Map Text LanguageString
    , appLinks       :: [Link]
    , appAnalytics   :: Analytics
    , appServices    :: AppServices
    , appCrossPost   :: AppCrossPost
    }
    deriving (Generic, Show)

instance Default AppData where
    def =
        AppData
            { appDirectory = def
            , appAddress = T.empty
            , appRealAddress = False
            , appArticles = def
            , appMeta = def
            , appStrings = def
            , appLinks = def
            , appAnalytics = def
            , appServices = def
            , appCrossPost = def
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
allLanguages :: AppData -> S.Set Language
allLanguages app = S.union articleLangs metaLangs
  where
    articleLangs = allContentLangs $ appArticles app
    metaLangs = allContentLangs $ appMeta app
    allContentLangs :: HasContent a => [a] -> S.Set Language
    allContentLangs = S.unions . map contentLangs
    contentLangs :: HasContent a => a -> S.Set Language
    contentLangs = S.fromList . M.keys . getContent

newtype AppCache =
    AppCache
        { appcachePdf :: Cache (Language, Text) LB.ByteString
        }

instance HasCache (Language, Text) LB.ByteString AppCache where
    getCache = appcachePdf

instance HasAppServices AppData where
    getAppServices = appServices

instance HasCrossPosts AppData where
    getCrossPosts = appCrossPost
