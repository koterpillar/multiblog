module Main where

import Control.Monad
import Control.Monad.State

import Data.Time

import Happstack.Server

import App
import Models
import Views


main :: IO ()
main = simpleHTTP' runApp nullConf $ msum [ mzero
                                          , index
                                          , yearlyIndex
                                          , monthlyIndex
                                          , dailyIndex
                                          , article
                                          ]

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

article :: ServerPartT App Response
article = dayPath $ \date -> path $ \slug -> do
    nullDir
    article <- getOne $ byDateSlug date slug
    articleDisplay article

articleList :: (Article -> Bool) -> ServerPartT App Response
articleList articleFilter = do
    articles <- getFiltered articleFilter
    articleListDisplay articles

dayPath :: (ServerMonad m, MonadPlus m) => (Day -> m b) -> m b
dayPath handler =
    path $ \year ->
    path $ \month ->
    path $ \day -> case fromGregorianValid year month day of
        Nothing -> mzero
        Just date -> handler date
