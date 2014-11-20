{-# Language OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State hiding (get)

import Data.Text.Lazy (Text)
import Data.Time

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty.Trans
import Web.Scotty.Internal.Types

import App
import Models
import Views

type AppAction a = ActionT Text App a

main :: IO ()
main = scottyT 8000 runApp runApp $ do
    get "/" index
    get "/:year" yearlyIndex
    get "/:year/:month" monthlyIndex
    get "/:year/:month/:day" dailyIndex
    get "/:year/:month/:day/:slug" article

index :: AppAction ()
index = articleList $ const True

yearlyIndex :: AppAction ()
yearlyIndex = do
    year <- param "year"
    articleList $ byYear year

monthlyIndex :: AppAction ()
monthlyIndex = do
    year <- param "year"
    month <- param "month"
    articleList $ byYearMonth year month

dateParam :: AppAction Day
dateParam = do
    year <- param "year"
    month <- param "month"
    day <- param "day"
    case fromGregorianValid year month day of
        Just date -> return date
        Nothing -> next

dailyIndex :: AppAction ()
dailyIndex = do
    date <- dateParam
    articleList $ byDate date

article :: AppAction ()
article = do
    date <- dateParam
    slug <- param "slug"
    -- TODO: getOne
    articles <- lift $ getFiltered $ byDateSlug date slug
    case articles of
        [article] -> html $ renderHtml $ articleDisplay article
        _ -> next

articleList :: (Article -> Bool) -> AppAction ()
articleList articleFilter = do
    articles <- lift $ getFiltered articleFilter
    html $ renderHtml $ articleListDisplay articles
