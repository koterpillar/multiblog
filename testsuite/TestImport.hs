{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module TestImport where

import Control.Monad

import qualified Data.ByteString.Char8 as C8
import Data.LanguageCodes
import Data.List
import qualified Data.Map as M
import Data.Text (pack)
import Data.Yaml

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

nullState :: AppState
nullState = AppState "" "" [] [] (M.empty)

testSource :: FilePath -> [(String, String)] -> String -> ContentSource
testSource path meta content = ContentSource path $ readMarkdown def $ markdown meta content

test_loadMeta = do
    let sources = [ testSource "meta/about.md" [slug "about", langEn] "This is meta"
                  ]
    assertEqual
        (Right $ nullState { appMeta = [ Meta { mtSlug = "about"
                                              , mtContent = M.fromList [ (EN, readMarkdown def "This is meta")
                                                                       ]
                                              }
                                       ]
                           })
        (liftM (modifyAppState textOnlyContent) $ fromSources sources)

jstring :: String -> Value
jstring = String . pack

test_loadStrings = do
    let strings = unlines [ "title:"
                          , "  en: Title"
                          , "  ru: Заголовок"
                          , "about:"
                          , "  zh: 关于"
                          ]
    assertEqual
        (loadStrings strings)
        (Right $ M.fromList [ ("title", M.fromList [(EN, "Title"), (RU, "Заголовок")])
                            , ("about", M.fromList [(ZH, "关于")])
                            ])

sort2 :: Ord a => [[a]] -> [[a]]
sort2 = sort . map sort

test_groupSources = do
    let snsd1en = testSource "2011-02-03/snsd.md" [slug "snsd", langEn] "Korean group"
    let snsd1ru = testSource "2011-02-03/snsd-ru.md" [slug "snsd", langRu] "Корейская группа"
    let snsd1zh = testSource "2011-02-03/snsd-zh.md" [slug "snsd", langZh] "韩国音乐组合"
    let snsd2en = testSource "2012-02-03/snsd.md" [slug "snsd", langEn] "Became popular"
    let aboutEn = testSource "meta/about.md" [slug "about", langEn] "Testing myself"
    let aboutRu = testSource "meta/about-ru.md" [slug "about", langRu] "Самопроверка"
    let sources = [snsd1en, snsd1ru, snsd1zh, snsd2en, aboutEn, aboutRu]
    assertEqual
        (sort2 [ [snsd1en, snsd1ru, snsd1zh]
               , [snsd2en]
               , [aboutEn, aboutRu]
               ])
        (sort2 $ M.elems $ groupSources sources)
