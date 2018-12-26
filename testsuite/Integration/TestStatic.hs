{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.TestStatic where

import Integration.Base

import Test.Framework

test_static = do
    resp <- makeRequestBS $ simpleRequest "/some-verification-file.html"
    assertEqual "This is the exact content of the verification file.\n" resp

test_static_index = do
    resp <- makeRequestBS $ simpleRequest "/with_index/"
    assertEqual "Index file\n" resp
