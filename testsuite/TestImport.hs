{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestImport where

import qualified Data.Map as M

import Text.Pandoc

import Import
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

modifyContent :: (Pandoc -> Pandoc) -> Article -> Article
modifyContent f a = a { arContent = M.map f (arContent a) }

textOnlyContent :: Article -> Article
textOnlyContent = modifyContent $ readMarkdown def . writePlain def

testArticle :: FilePath -> [(String, String)] -> String -> ArticleSource
testArticle path meta content = ArticleSource path $ readMarkdown def $ markdown meta content

test_groupArticles = do
    let sources = [ testArticle "2011-02-03/snsd.md" [slug "snsd", langEn] "Korean group"
                  , testArticle "2011-02-03/snsd-ru.md" [slug "snsd", langRu] "Корейская группа"
                  , testArticle "2011-02-03/snsd-zh.md" [slug "snsd", langZh] "韩国音乐组合"
                  ]
    assertEqual
        [ Article { arSlug = "snsd"
                  , arContent = M.fromList [ ("en", readMarkdown def "Korean group")
                                           , ("ru", readMarkdown def "Корейская группа")
                                           , ("zh", readMarkdown def "韩国音乐组合")
                                           ]
                  , arAuthored = mkDate 2011 02 03
                  }
        ]
        (map textOnlyContent $ groupArticles sources)
