{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestImport where

import Control.Monad.State

import qualified Data.ByteString.UTF8 as U
import Data.LanguageCodes
import qualified Data.Map as M
import Data.Yaml

import Text.Pandoc hiding (Meta)
import Text.Pandoc.Error

import Import
import Models

import Test.Framework

markdown :: [(String, String)] -> String -> String
markdown meta content =
    unlines $
    ["---"] ++
    [ k ++ ": " ++ v
    | (k, v) <- meta ] ++
    ["---", content]

unsafeReadMarkdown :: String -> Pandoc
unsafeReadMarkdown = handleError . readMarkdown def

slug s = ("slug", s)

langEn, langRu, langZh :: (String, String)
langEn = ("lang", "en")

langRu = ("lang", "ru")

langZh = ("lang", "zh")

modifyAllContent
    :: HasContent a
    => (Pandoc -> Pandoc) -> a -> a
modifyAllContent f = modifyContent (M.map f)

textOnlyContent
    :: HasContent a
    => a -> a
textOnlyContent = modifyAllContent $ unsafeReadMarkdown . writePlain def

modifyAppData
    :: (forall a. HasContent a =>
                  a -> a)
    -> AppData
    -> AppData
modifyAppData f st =
    st
    { appArticles = map f $ appArticles st
    , appMeta = map f $ appMeta st
    }

testSource :: FilePath -> [(String, String)] -> String -> ContentSource
testSource path meta content =
    ContentSource path $ unsafeReadMarkdown $ markdown meta content

test_loadMeta = do
    let sources =
            [testSource "meta/about.md" [slug "about", langEn] "This is meta"]
    let Right (articles, meta) = fromSources sources
    assertEqual [] articles
    assertEqual
        [ Meta
          { mtSlug = "about"
          , mtContent = M.fromList [(EN, unsafeReadMarkdown "This is meta")]
          }
        ]
        (map textOnlyContent meta)

test_loadMeta_implied = do
    let sources = [testSource "meta/about-en.md" [] "This is meta"]
    let Right (articles, meta) = fromSources sources
    assertEqual [] articles
    assertEqual
        [ Meta
          { mtSlug = "about"
          , mtContent = M.fromList [(EN, unsafeReadMarkdown "This is meta")]
          }
        ]
        (map textOnlyContent meta)

test_loadArticle = do
    let sources =
            [ testSource
                  "2015-03-01/world-order-en.md"
                  []
                  "Should be parsed automatically"
            ]
    let Right (articles, meta) = fromSources sources
    assertEqual [] meta
    assertEqual
        [ Article
          { arSlug = "world-order"
          , arAuthored = mkDate 2015 03 01
          , arContent =
              M.fromList
                  [(EN, unsafeReadMarkdown "Should be parsed automatically")]
          }
        ]
        (map textOnlyContent articles)

test_extractLanguage = do
    let runExtract = runState extractLanguage
    assertEqual (Just CS, []) $ runExtract [Unnamed "cs"]
    assertEqual (Nothing, [Unnamed "zzz"]) $ runExtract [Unnamed "zzz"]
    assertEqual (Just CS, []) $ runExtract [Named "lang" "cs"]
    assertEqual (Nothing, [Named "lang" "ttt"]) $
        runExtract [Named "lang" "ttt"]
    assertEqual (Just CS, [Unnamed "something"]) $
        runExtract [Unnamed "something-cs"]

test_loadStrings = do
    let strings =
            U.fromString $
            unlines
                [ "title:"
                , "  en: Title"
                , "  ru: Заголовок"
                , "about:"
                , "  zh: 关于"
                ]
    assertEqual
        (Right $
         M.fromList
             [ ("title", M.fromList [(EN, "Title"), (RU, "Заголовок")])
             , ("about", M.fromList [(ZH, "关于")])
             ])
        (decodeEither strings :: Either String (M.Map String LanguageString))

test_loadLinks = do
    let links =
            U.fromString $
            unlines
                [ "- page: about"
                , "- url: https://1.example.com/"
                , "  text: Example 1"
                , "- url: https://2.example.com/"
                , "  text:"
                , "    en: Example 2"
                , "    ru: Пример 2"
                , "    zh: 列子2"
                ]
    assertEqual
        (Right $
         [ MetaLink "about"
         , ExternalLink
               "https://1.example.com/"
               (M.fromList [(EN, "Example 1")])
         , ExternalLink
               "https://2.example.com/"
               (M.fromList [(EN, "Example 2"), (RU, "Пример 2"), (ZH, "列子2")])
         ])
        (decodeEither links :: Either String [Link])

test_loadCrossPost = do
    let crossPosts =
            U.fromString $
            unlines
                [ "- service: twitter"
                , "  lang: es"
                , "  oauth_token: ABCDE"
                , "  oauth_token_secret: FGHIJ"
                ]
    assertEqual
        (Right $
         [ CrossPost
           { cpLanguage = ES
           , cpServiceDetails = TwitterAuth "ABCDE" "FGHIJ"
           }
         ])
        (decodeEither crossPosts :: Either String [CrossPost])
