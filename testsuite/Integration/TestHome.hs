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
