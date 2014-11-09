{-# Language FlexibleContexts #-}
module App where

import Control.Monad
import Control.Monad.State

import Data.Time

import Happstack.Server

import Models


data AppState = AppState { appArticles :: [Article]
                         }

type App = StateT AppState IO

runApp :: App a -> IO a
runApp a = do
    let initialState = AppState { appArticles = [someArticle]
                                }
    evalStateT a initialState
    where someArticle = Article "test" "test article" someDate
          someDate = UTCTime (fromGregorian 2014 11 08) 0

getApp :: MonadState AppState m => m AppState
getApp = get

getFiltered :: MonadState AppState m => (Article -> Bool) -> m [Article]
getFiltered articleFilter = gets $ filter articleFilter . appArticles

getOne :: (MonadState AppState m, MonadPlus m) =>
    (Article -> Bool) -> m Article
getOne articleFilter = do
    articles <- getFiltered articleFilter
    case articles of
        [article] -> return article
        _ -> mzero
