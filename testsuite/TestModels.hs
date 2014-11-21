{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestModels where

import qualified Data.Map as M

import Models

import Test.Framework


markdown :: [(String, String)] -> String -> String
markdown meta content = unlines $
    ["---"] ++
    [k ++ ": " ++ v | (k, v) <- meta] ++
    ["---", content]

slug s = ("slug", s)
langEn = ("lang", "en")
langRu = ("lang", "ru")
langZh = ("lang", "zh")

contentIrrelevant :: Article -> Article
contentIrrelevant a = a { arContent = M.empty }


test_fromDirectory = do
    let articles = [ ("2011-02-03/snsd.md", markdown [slug "snsd", langEn] "Korean group")
                   , ("2011-02-03/snsd-ru.md", markdown [slug "snsd", langRu] "Корейская группа")
                   , ("2011-02-03/snsd-zh.md", markdown [slug "snsd", langZh] "韩国音乐组合")
                   ]
    assertEqual
        [ Article { arSlug = "snsd"
                  , arContent = M.fromList [ ("en", "Korean group")
                                           , ("ru", "Корейская группа")
                                           , ("zh", "韩国音乐组合")
                                           ]
                  , arAuthored = mkDate 2011 02 03
                  }
        ]
        fromFiles articles
