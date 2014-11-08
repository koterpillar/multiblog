{-# Language FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State

import Data.Time

import Happstack.Server


main :: IO ()
main = simpleHTTP' runApp nullConf $ msum [ mzero
                                          , index
                                          , yearlyIndex
                                          , monthlyIndex
                                          , dailyIndex
                                          , article
                                          ]

data Article = Article { arTitle :: String
                       , arContent :: Content
                       , arAuthored :: UTCTime
                       }

type Content = String

data AppState = AppState { appArticles :: [Article]
                         }

getFiltered :: MonadState AppState m => (Article -> Bool) -> m [Article]
getFiltered articleFilter = gets $ filter articleFilter . appArticles

getOne :: (MonadState AppState m, MonadPlus m) =>
    (Article -> Bool) -> m Article
getOne articleFilter = do
    articles <- getFiltered articleFilter
    case articles of
        [article] -> return article
        _ -> mzero

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

type App = StateT AppState IO

runApp :: App a -> IO a
runApp a = evalStateT a initialState
    where initialState = AppState [ someArticle
                                  ]
          someArticle = Article "test" "test article" someDate
          someDate = UTCTime (fromGregorian 2014 11 08) 0

index :: ServerPartT App Response
index = do
    nullDir
    articleList $ const True

yearlyIndex :: ServerPartT App Response
yearlyIndex = path $ \year -> do
    nullDir
    articleList $ byYear year

monthlyIndex :: ServerPartT App Response
monthlyIndex = path $ \year -> path $ \ month -> do
    nullDir
    articleList $ byYearMonth year month

dailyIndex :: ServerPartT App Response
dailyIndex = dayPath $ \date -> do
    nullDir
    articleList $ byDate date

articleList :: (Article -> Bool) -> ServerPartT App Response
articleList articleFilter = do
    articles <- getFiltered articleFilter
    articleListDisplay articles

articleListDisplay :: [Article] -> ServerPartT App Response
articleListDisplay = ok . toResponse . show . map arContent

dayPath :: (ServerMonad m, MonadPlus m) => (Day -> m b) -> m b
dayPath handler =
    path $ \year ->
    path $ \month ->
    path $ \day -> case fromGregorianValid year month day of
        Nothing -> mzero
        Just date -> handler date

article :: ServerPartT App Response
article = dayPath $ \date -> path $ \slug -> do
    nullDir
    article <- getOne $ byDateSlug date slug
    articleDisplay article

articleDisplay :: Article -> ServerPartT App Response
articleDisplay = ok . toResponse . arContent
