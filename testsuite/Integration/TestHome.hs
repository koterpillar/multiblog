{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestHome where

import Control.Monad

import Data.LanguageCodes

import Test.Framework

import Integration.Base

homeRequest = simpleRequest "/"

test_home = do
    home <- makeRequest $ homeRequest
    assertContains "Test site" home
    -- Articles must be ordered correctly
    assertContainsBefore "Another article" "First test article" home
    -- This article must be on the second page
    assertContains "Next page" home
    assertNotContains "Previous page" home
    assertNotContains "Very early article" home
    assertContains "<a href=\"http://test/about\">Test About</a>" home
    assertContains "<a href=\"https://1.example.com/\">Example 1</a>" home
    assertContains "<a href=\"https://2.example.com/\">Example 2</a>" home

test_home_lang = do
    home <- makeRequest $ withLang1 RU $ homeRequest
    assertContains "Главная" home
    assertContains "<a href=\"https://1.example.com/\">Example 1</a>" home
    assertContains "<a href=\"https://2.example.com/\">Пример 2</a>" home

test_explicit_lang = do
    home <- makeRequest $ simpleRequest "/?lang=ru"
    assertContains "Главная" home

test_cookie_lang = do
    home <- makeRequest $ withLangCookie ZH $ homeRequest
    assertContains "<a href=\"http://test/about\">测试关于页</a>" home
    assertContains "首页" home

test_home_next_page = do
    home <- makeRequest $ simpleRequest "/?page=2"
    assertContains "Very early article" home
    assertContains "Previous page" home
    assertNotContains "Next page" home
