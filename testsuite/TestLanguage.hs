{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

module TestLanguage where

import qualified Data.Map as M

import Language

import Test.Framework


test_matchLanguageFunc = do
    let values = M.fromList [ ("en", "English")
                            , ("ru", "Russian")
                            , ("zh", "Chinese")
                            ]
    assertEqual
        (Nothing :: Maybe String)
        (matchLanguage (M.fromList [("ru", 1)]) M.empty)
    assertEqual
        (Just "Russian")
        (matchLanguage (M.fromList [("ru", 1)]) values)

test_languageHeader = do
    assertEqual
        (languageHeader Nothing)
        (M.fromList [("en", 1)])
    assertEqual
        (languageHeader $ Just "fr")
        (M.fromList [("fr", 1)])
    assertEqual
        (languageHeader $ Just "de,fr,ko")
        (M.fromList [("de", 1), ("fr", 1), ("ko", 1)])
    assertEqual
        (languageHeader $ Just "ru,zh;q=0.8,en;q=0.6")
        (M.fromList [("ru", 1), ("zh", 0.8), ("en", 0.6)])
