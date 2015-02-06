{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestHome where

import Control.Monad

import Data.LanguageCodes

import Test.Framework

import Integration.Base


test_home = do
    req <- mkRequest "/"
    home <- testResponse req
    assertContains "Test site" home
    assertContainsBefore "Another article" "First test article" home

test_home_lang = do
    req <- liftM (withLang1 RU) (mkRequest "/")
    home <- testResponse req
    assertContains "Главная" home

test_explicit_lang = do
    req <- mkRequest "/?lang=ru"
    home <- testResponse req
    assertContains "Главная" home
