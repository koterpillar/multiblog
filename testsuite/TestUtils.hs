{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

module TestUtils where

import qualified Data.Map as M

import Utils

import Test.Framework

test_mapAllRight = do
    assertEqual
        (Right $ M.fromList [("a", 1), ("b", 2)] :: Either String (M.Map String Int))
        (mapAllRight $ M.fromList [("a", Right 1), ("b", Right 2)])
    assertEqual
        (Left "First error")
        (mapAllRight $ M.fromList [ ("a", Right 1)
                                  , ("b", Left "First error")
                                  , ("c", Right 2)
                                  , ("d", Left "Second error")
                                  ])
