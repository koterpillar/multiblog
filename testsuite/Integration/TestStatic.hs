{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Integration.TestStatic where

import Control.Monad

import Types.Language

import Integration.Base

import Test.Framework

test_static = do
    resp <- makeRequest $ simpleRequest "/some-verification-file.html"
    assertEqual "This is the exact content of the verification file.\n" resp

test_static_index = do
    resp <- makeRequest $ simpleRequest "/with_index/"
    assertEqual "Index file\n" resp
