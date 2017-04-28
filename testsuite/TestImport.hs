{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestImport where

import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.ByteString.UTF8 as U
import Data.LanguageCodes
import qualified Data.Map as M
import Data.Yaml

import Text.Pandoc hiding (Meta)

import Import
import Models
import Types.Content
import Types.Services

import Test.Framework

unsafeReadMarkdown :: String -> Pandoc
unsafeReadMarkdown = handleError . readMarkdown def

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

test_loadMeta = do
    let directory =
            SourceDirectory
            { sdName = "about"
            , sdFiles =
                  [ SourceFile {sfName = "en.md", sfContent = "This is meta"}
                  , SourceFile {sfName = "ru.md", sfContent = "Это мета"}
                  ]
            }
    let (Identity result) = runExceptT $ parseMeta directory
    assertEqual
        (Right
             (Meta
              { mtSlug = "about"
              , mtLayout = BaseLayout
              , mtContent =
                    M.fromList
                        [ (EN, unsafeReadMarkdown "This is meta")
                        , (RU, unsafeReadMarkdown "Это мета")
                        ]
              }))
        result

test_loadMetaTalkLayout = do
    let directory =
            SourceDirectory
            { sdName = "talk"
            , sdFiles =
                  [SourceFile {sfName = "en.md", sfContent = "Talk content"}]
            }
    let (Identity result) = runExceptT $ parseMeta directory
    assertEqual
        (Right
             (Meta
              { mtSlug = "talk"
              , mtLayout = TalkLayout
              , mtContent = M.fromList [(EN, unsafeReadMarkdown "Talk content")]
              }))
        result

test_loadArticle = do
    let directory =
            SourceDirectory
            { sdName = "2015-03-01-article-one"
            , sdFiles =
                  [ SourceFile {sfName = "en.md", sfContent = "Article One"}
                  , SourceFile {sfName = "ru.md", sfContent = "Статья Один"}
                  ]
            }
    let (Identity result) = runExceptT $ parseArticle directory
    assertEqual
        (Right
             (Article
              { arSlug = "article-one"
              , arAuthored = mkDate 2015 03 01
              , arContent =
                    M.fromList
                        [ (EN, unsafeReadMarkdown "Article One")
                        , (RU, unsafeReadMarkdown "Статья Один")
                        ]
              }))
        result

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
           , cpServiceDetails = AppAuthTwitter (TwitterAuth "ABCDE" "FGHIJ")
           }
         ])
        (decodeEither crossPosts :: Either String AppCrossPost)
