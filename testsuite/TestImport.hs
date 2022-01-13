{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module TestImport where

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.LanguageCodes
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Yaml

import           Text.Pandoc            hiding (Meta)

import           Import
import           Models
import           Types.Content

import           Test.HUnit

unsafeReadMarkdown :: Text -> Pandoc
unsafeReadMarkdown = runPandocPure' . readMarkdown def

modifyAllContent :: HasContent a => (Pandoc -> Pandoc) -> a -> a
modifyAllContent = modifyContent . fmap

modifyAppData ::
       (forall a. HasContent a =>
                      a -> a)
    -> AppData
    -> AppData
modifyAppData f st =
    st {appArticles = map f $ appArticles st, appMeta = map f $ appMeta st}

testSource :: Text -> Text -> SourceFile
testSource name content =
    SourceFile {sfName = name, sfContent = Text.encodeUtf8 content}

defaultMeta =
    Meta
        { mtSlug = ""
        , mtLayout = BaseLayout
        , mtExportSlugOverride = Nothing
        , mtContent = Map.empty
        }

test_loadMeta = do
    let directory =
            SourceDirectory
                { sdName = "about"
                , sdFiles =
                      [ testSource "en.md" "This is meta"
                      , testSource "ru.md" "Это мета"
                      ]
                }
    let (Identity result) = runExceptT $ parseMeta directory
    assertEqual
        ""
        (Right
             defaultMeta
                 { mtSlug = "about"
                 , mtContent =
                       Map.fromList
                           [ (EN, unsafeReadMarkdown "This is meta")
                           , (RU, unsafeReadMarkdown "Это мета")
                           ]
                 })
        result

test_loadMetaPresentationLayout = do
    let directory =
            SourceDirectory
                { sdName = "talk"
                , sdFiles =
                      [ testSource "en.md" "Talk content"
                      , testSource "options.yaml" "layout: presentation"
                      ]
                }
    let (Identity result) = runExceptT $ parseMeta directory
    assertEqual
        ""
        (Right
             defaultMeta
                 { mtSlug = "talk"
                 , mtLayout = PresentationLayout
                 , mtContent =
                       Map.fromList [(EN, unsafeReadMarkdown "Talk content")]
                 })
        result

test_loadMetaExportSlug = do
    let directory =
            SourceDirectory
                { sdName = "resume"
                , sdFiles =
                      [ testSource "en.md" "Resume content"
                      , testSource "options.yaml" "exportSlug: MyName"
                      ]
                }
    let (Identity result) = runExceptT $ parseMeta directory
    assertEqual
        ""
        (Right
             defaultMeta
                 { mtSlug = "resume"
                 , mtExportSlugOverride = Just "MyName"
                 , mtContent =
                       Map.fromList [(EN, unsafeReadMarkdown "Resume content")]
                 })
        result

test_loadArticle = do
    let directory =
            SourceDirectory
                { sdName = "2015-03-01-article-one"
                , sdFiles =
                      [ testSource "en.md" "Article One"
                      , testSource "ru.md" "Статья Один"
                      ]
                }
    let (Identity result) = runExceptT $ parseArticle directory
    assertEqual
        ""
        (Right
             Article
                 { arSlug = "article-one"
                 , arAuthored = mkDate 2015 03 01
                 , arContent =
                       Map.fromList
                           [ (EN, unsafeReadMarkdown "Article One")
                           , (RU, unsafeReadMarkdown "Статья Один")
                           ]
                 })
        result

test_loadStrings = do
    let strings =
            Text.encodeUtf8 $
            Text.unlines
                [ "title:"
                , "  en: Title"
                , "  ru: Заголовок"
                , "about:"
                , "  zh: 关于"
                ]
    decodeThrow strings >>=
        assertEqual
            ""
            (Map.fromList
                 [ ("title", Map.fromList [(EN, "Title"), (RU, "Заголовок")])
                 , ("about", Map.fromList [(ZH, "关于")])
                 ] :: Map.Map String LanguageString)

test_loadLinks = do
    let links =
            Text.encodeUtf8 $
            Text.unlines
                [ "- page: about"
                , "- url: https://1.example.com/"
                , "  text: Example 1"
                , "- url: https://2.example.com/"
                , "  text:"
                , "    en: Example 2"
                , "    ru: Пример 2"
                , "    zh: 列子2"
                ]
    decodeThrow links >>=
        assertEqual
            ""
            [ MetaLink "about"
            , ExternalLink
                  "https://1.example.com/"
                  (Map.fromList [(EN, "Example 1")])
            , ExternalLink
                  "https://2.example.com/"
                  (Map.fromList
                       [(EN, "Example 2"), (RU, "Пример 2"), (ZH, "列子2")])
            ]
