{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.State hiding (get)

import Data.Text.Lazy (Text, unpack)
import Data.Time

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty.Trans

import App
import Language
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

languageHeaderM :: AppAction LanguagePreference
languageHeaderM = liftM (languageHeader . liftM unpack) $ header "Accept-Language"

article :: AppAction ()
article = do
    date <- dateParam
    slug <- param "slug"
    language <- languageHeaderM
    -- TODO: getOne
    articles <- lift $ getFiltered $ byDateSlug date slug
    case articles of
        [a] -> html $ renderHtml $ articleDisplay language a
        _ -> next

articleList :: (Article -> Bool) -> AppAction ()
articleList articleFilter = do
    articles <- lift $ getFiltered articleFilter
    language <- languageHeaderM
    html $ renderHtml $ articleListDisplay language articles
