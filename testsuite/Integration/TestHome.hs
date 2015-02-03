{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestHome where

import Control.Monad

import Data.LanguageCodes

import Test.Framework

import Integration.Base


test_home = do
    req <- mkRequest "/"
    home <- testRequest req
    resp <- responseContent home
    assertContains "Test site" resp

test_home_lang = do
    req <- liftM (withLang1 RU) (mkRequest "/")
    home <- testRequest req
    resp <- responseContent home
    assertContains "Главная" resp

test_explicit_lang = do
    req <- mkRequest "/?lang=ru"
    home <- testRequest req
    resp <- responseContent home
    assertContains "Главная" resp

test_cookie_lang = do
    req <- liftM (withLangCookie ZH) (mkRequest "/")
    home <- testRequest req
    resp <- responseContent home
    assertContains "首页" resp
