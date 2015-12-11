{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestRoutes where

import Routes

import Test.Framework

test_splitExt = do
    assertEqual
        ("segment", Nothing)
        (splitExt "segment")
    assertEqual
        ("segment", Just "ext")
        (splitExt "segment.ext")
    assertEqual
        ("segment.more.dots", Just "ext")
        (splitExt "segment.more.dots.ext")

prop_splitExt_joinExt seg = let (seg', ext) = splitExt seg in joinExt seg' ext == seg
