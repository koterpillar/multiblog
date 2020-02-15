{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestHome where

import           Data.LanguageCodes

import           Test.Framework

import           Integration.Base

homeRequest :: TestRequest
homeRequest = simpleRequest "/"

test_home :: IO ()
test_home = do
    home <- makeRequestText homeRequest
    assertTextContains "Test site" home
    -- Articles must be ordered correctly
    assertTextContainsBefore "Another article" "First test article" home
    -- This article must be on the second page
    assertTextContains "Next page" home
    assertTextNotContains "Previous page" home
    assertTextNotContains "Very early article" home
    assertTextContains "<a href=\"http://test/about\">Test About</a>" home
    assertTextContains "<a href=\"https://1.example.com/\">Example 1</a>" home
    assertTextContains "<a href=\"https://2.example.com/\">Example 2</a>" home

test_home_lang :: IO ()
test_home_lang = do
    home <- makeRequestText $ withLang1 RU homeRequest
    assertTextContains "Главная" home
    assertTextContains "<a href=\"https://1.example.com/\">Example 1</a>" home
    assertTextContains "<a href=\"https://2.example.com/\">Пример 2</a>" home

test_explicit_lang :: IO ()
test_explicit_lang = do
    home <- makeRequestText $ simpleRequest "/?lang=ru"
    assertTextContains "Главная" home

test_cookie_lang :: IO ()
test_cookie_lang = do
    home <- makeRequestText $ withLangCookie ZH homeRequest
    assertTextContains "<a href=\"http://test/about\">测试关于页</a>" home
    assertTextContains "首页" home

test_home_next_page :: IO ()
test_home_next_page = do
    home <- makeRequestText $ simpleRequest "/?page=2"
    assertTextContains "Very early article" home
    assertTextContains "Previous page" home
    assertTextNotContains "Next page" home

test_css :: IO ()
test_css = do
    css <- makeRequestText $ simpleRequest "/assets/code.css"
    assertTextContains "color" css

test_js :: IO ()
test_js = do
    js <- makeRequestText $ simpleRequest "/assets/site.js"
    assertTextContains "function" js
