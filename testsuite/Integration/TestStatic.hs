{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestStatic where

import Control.Monad

import Language

import Integration.Base

import Test.Framework


test_static = do
    resp <- makeRequest $ simpleRequest "/some-verification-file.html"
    assertEqual
        "This is the exact content of the verification file.\n"
        resp
