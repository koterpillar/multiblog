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

-- A route that matches on multiple path segments
class Route a where
    route :: (ServerMonad m, MonadPlus m) => (a -> m b) -> m b

newtype Path p = Path p

instance FromReqURI a => Route (Path a) where
    route h = path $ h . Path

instance (Route a, Route b) => Route (a, b) where
    route handler = route $ \a -> route $ \b -> handler (a, b)

instance (Route a, Route b, Route c) => Route (a, b, c) where
    route handler = route $ \a -> route $ \b -> route $ \c -> handler (a, b, c)

instance Route Day where
    route handler = route $ \(Path y, Path m, Path d) ->
        case fromGregorianValid y m d of
            Nothing -> mzero
            Just date -> handler date

index :: ServerPartT App Response
index = do
    nullDir
    articleList $ const True

yearlyIndex :: ServerPartT App Response
yearlyIndex = route $ \(Path year) -> do
    nullDir
    articleList $ byYear year

monthlyIndex :: ServerPartT App Response
monthlyIndex = route $ \(Path year, Path month) -> do
    nullDir
    articleList $ byYearMonth year month

dailyIndex :: ServerPartT App Response
dailyIndex = route $ \date -> do
    nullDir
    articleList $ byDate date

article :: ServerPartT App Response
article = route $ \(date , Path slug) -> do
    nullDir
    article <- getOne $ byDateSlug date slug
    articleDisplay article

articleList :: (Article -> Bool) -> ServerPartT App Response
articleList articleFilter = do
    articles <- getFiltered articleFilter
    articleListDisplay articles
