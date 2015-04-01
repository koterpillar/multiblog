{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE RankNTypes #-}

module TestImport where

import Control.Monad
import Control.Monad.State

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
import Test.HUnit


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
    let Right (articles, meta) = fromSources sources
    articles @?= []
    map textOnlyContent meta @?= [Meta { mtSlug = "about"
                                       , mtContent = M.fromList [ (EN, readMarkdown def "This is meta")
                                                                ]
                                       }]

test_loadArticle = do
    let sources = [ testSource "2015-03-01/world-order-en.md" [] "Should be parsed automatically"
                  ]
    let Right (articles, meta) = fromSources sources
    articles @?= [Article { arSlug = "world-order"
                          , arAuthored = mkDate 2015 03 01
                          , arContent = M.fromList [ (EN, readMarkdown def "Should be parsed automatically")
                                                   ]
                          }]
    meta @?= []

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

test_extractLanguage = do
    let runExtract = runState extractLanguage
    runExtract [Unnamed "cs"] @?= (Just CS, [])
    runExtract [Unnamed "zzz"] @?= (Nothing, [Unnamed "zzz"])
    runExtract [Named "lang" "cs"] @?= (Just CS, [])
    runExtract [Named "lang" "ttt"] @?= (Nothing, [Named "lang" "ttt"])
    runExtract [Unnamed "something-cs"] @?= (Just CS, [Unnamed "something"])
