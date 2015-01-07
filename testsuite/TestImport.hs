{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

module TestImport where

import Control.Monad

import Data.List
import qualified Data.Map as M

import Text.Pandoc hiding (Meta)

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

modifyAllContent :: HasContent a => (Pandoc -> Pandoc) -> a -> a
modifyAllContent f = modifyContent (M.map f)

textOnlyContent :: HasContent a => a -> a
textOnlyContent = modifyAllContent $ readMarkdown def . writePlain def

modifyAppState :: (forall a. HasContent a => a -> a) -> AppState -> AppState
modifyAppState f st = st { appArticles = map f $ appArticles st
                         , appMeta = map f $ appMeta st
                         }

testArticle :: FilePath -> [(String, String)] -> String -> ContentSource
testArticle path meta content = ContentSource path $ readMarkdown def $ markdown meta content

test_loadMeta = do
    let sources = [ testArticle "meta/about.md" [slug "about", langEn] "This is meta"
                  ]
    assertEqual
        (Right $ AppState [] [ Meta { mtSlug = "about"
                             , mtContent = M.fromList [ ("en", readMarkdown def "This is meta")
                                                      ]
                                    }
                             ])
        (liftM (modifyAppState textOnlyContent) $ fromSources sources)

sort2 :: Ord a => [[a]] -> [[a]]
sort2 = sort . map sort

test_groupSources = do
    let snsd1en = testArticle "2011-02-03/snsd.md" [slug "snsd", langEn] "Korean group"
    let snsd1ru = testArticle "2011-02-03/snsd-ru.md" [slug "snsd", langRu] "Корейская группа"
    let snsd1zh = testArticle "2011-02-03/snsd-zh.md" [slug "snsd", langZh] "韩国音乐组合"
    let snsd2 = testArticle "2012-02-03/snsd.md" [slug "snsd", langEn] "Became popular"
    let aboutEn = testArticle "meta/about.md" [slug "about", langEn] "Testing myself"
    let aboutRu = testArticle "meta/about-ru.md" [slug "about", langRu] "Самопроверка"
    let sources = [snsd1en, snsd1ru, snsd1zh, snsd2, aboutEn, aboutRu]
    assertEqual
        (sort2 [ [snsd1en, snsd1ru, snsd1zh]
               , [snsd2]
               , [aboutEn, aboutRu]
               ])
        (sort2 $ M.elems $ groupSources sources)
