{-# LANGUAGE OverloadedStrings #-}

module Integration.TestStatic where

import           Integration.Base
import           Test.HUnit

test_static :: IO ()
test_static = do
    resp <- makeRequestBS $ simpleRequest "/some-verification-file.html"
    assertEqual "" "This is the exact content of the verification file.\n" resp

test_static_index :: IO ()
test_static_index = do
    resp <- makeRequestBS $ simpleRequest "/with_index/"
    assertEqual "" "Index file\n" resp
