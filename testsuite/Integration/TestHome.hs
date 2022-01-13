{-# LANGUAGE OverloadedStrings #-}

module Integration.TestHome where

import           Data.LanguageCodes

import           Integration.Base

homeRequest :: TestRequest
homeRequest = simpleRequest "/"

unit_home :: IO ()
unit_home = do
    home <- makeRequestText homeRequest
    home `shouldContainText` "Test site"
    -- Articles must be ordered correctly
    assertTextContainsBefore "Another article" "First test article" home
    -- This article must be on the second page
    home `shouldContainText` "Next page"
    home `shouldNotContainText` "Previous page"
    home `shouldNotContainText` "Very early article"
    home `shouldContainText` "<a href=\"http://test/about\">Test About</a>"
    home `shouldContainText` "<a href=\"https://1.example.com/\">Example 1</a>"
    home `shouldContainText` "<a href=\"https://2.example.com/\">Example 2</a>"

unit_home_lang :: IO ()
unit_home_lang = do
    home <- makeRequestText $ withLang1 RU homeRequest
    home `shouldContainText` "Главная"
    home `shouldContainText` "<a href=\"https://1.example.com/\">Example 1</a>"
    home `shouldContainText` "<a href=\"https://2.example.com/\">Пример 2</a>"

unit_explicit_lang :: IO ()
unit_explicit_lang = do
    home <- makeRequestText $ simpleRequest "/?lang=ru"
    home `shouldContainText` "Главная"

unit_cookie_lang :: IO ()
unit_cookie_lang = do
    home <- makeRequestText $ withLangCookie ZH homeRequest
    home `shouldContainText` "<a href=\"http://test/about\">测试关于页</a>"
    home `shouldContainText` "首页"

unit_home_next_page :: IO ()
unit_home_next_page = do
    home <- makeRequestText $ simpleRequest "/?page=2"
    home `shouldContainText` "Very early article"
    home `shouldContainText` "Previous page"
    home `shouldNotContainText` "Next page"

unit_css :: IO ()
unit_css = do
    css <- makeRequestText $ simpleRequest "/assets/code.css"
    css `shouldContainText` "color"

unit_js :: IO ()
unit_js = do
    js <- makeRequestText $ simpleRequest "/assets/site.js"
    js `shouldContainText` "function"
