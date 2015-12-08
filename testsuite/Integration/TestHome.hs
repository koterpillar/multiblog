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
    assertContainsBefore "Another article" "First test article" home

test_home_lang = do
    home <- makeRequest $ withLang1 RU $ homeRequest
    assertContains "Главная" home

test_explicit_lang = do
    home <- makeRequest $ simpleRequest "/?lang=ru"
    assertContains "Главная" home

test_cookie_lang = do
    home <- makeRequest $ withLangCookie ZH $ homeRequest
    assertContains "首页" home
