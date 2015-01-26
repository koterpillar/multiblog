{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestHome where

import Integration.Base

import Test.Framework


test_home = do
    req <- mkRequest "/"
    home <- testRequest req
    resp <- responseContent home
    assertContains "Test site" resp
