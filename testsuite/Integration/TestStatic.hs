{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestStatic where

import Control.Monad

import Language

import Integration.Base

import Test.Framework


test_static = do
    req <- mkRequest "/some-verification-file.html"
    static <- testRequest req
    resp <- responseContent static
    assertEqual
        "This is the exact content of the verification file.\n"
        resp
