{-# Language FlexibleContexts #-}
module App where

import Control.Monad
import Control.Monad.State

import Data.Time

import qualified Web.Scotty as S

import Import
import Models


data AppState = AppState { appArticles :: [Article]
                         }

type App = StateT AppState IO

runApp :: App a -> IO a
runApp a = do
    articles <- fromDirectory "content"
    let initialState = AppState { appArticles = articles
                                }
    evalStateT a initialState

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
