{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestBase where

import Integration.Base

import Test.Framework


test_home = do
    req <- mkRequest "/"
    home <- testRequest req
    assertContains "Test site" $ responseContent home
