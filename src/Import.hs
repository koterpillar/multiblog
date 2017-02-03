{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- Import the application data (articles and meta) from a set of files.
module Import where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.ByteString as B
import Data.Default.Class
import Data.Either
import qualified Data.Map as M
import Data.Time
import qualified Data.Yaml as Y

import Text.Pandoc hiding (Meta, readers)
import Text.Pandoc.Error

import System.Directory
import System.FilePath.Posix

import Models
import Types.Content
import Types.Language
import Utils

-- Each file gets imported into a ContentSource
data ContentSource = ContentSource
    { csPath :: FilePath
    , csContent :: Pandoc
    } deriving (Eq, Ord, Show)

-- Each language group of ContentSources becomes either Article or Meta
-- later
data Content = Content
    { coSlug :: String
    , coDate :: Maybe UTCTime
    , coContent :: LanguageContent
    } deriving (Eq, Ord, Show)

-- Merge content belonging to the same article/meta
mergeContent :: Content -> Content -> Content
mergeContent c1 c2 =
    Content
    { coSlug = coSlug c1
    , coDate = coDate c1
    , coContent = M.union (coContent c1) (coContent c2)
    }

parseContent :: Content -> Either Article Meta
parseContent c =
    case coDate c of
        Just dt -> Left $ Article (coSlug c) (coContent c) dt
        Nothing -> Right $ Meta (coSlug c) (coContent c)

extractContent :: ContentSource -> Either String Content
extractContent cs = do
    let path = csPath cs
    let pathComponents = splitDirectories $ dropExtension path
    let makeContent :: String
                    -> Maybe UTCTime
                    -> String
                    -> Either String Content
        makeContent slug date langStr = do
            lang <- parseLanguage langStr
            return $ Content slug date (M.singleton lang $ csContent cs)
    case pathComponents of
        ["meta", slug, langStr] -> makeContent slug Nothing langStr
        [slugDate, langStr] ->
            let directoryNameError =
                    Left $ "Invalid article directory name format: " ++ slugDate
            in case reads slugDate of
                   [(day, slugDateRest)] ->
                       case slugDateRest of
                           '-':slug ->
                               makeContent slug (Just $ atMidnight day) langStr
                           _ -> directoryNameError
                   _ -> directoryNameError
        _ ->
            Left $ "File path corresponds to neither article nor meta: " ++ path

extractSlugDateLang :: FilePath
                    -> Either String (String, Maybe UTCTime, Language)
extractSlugDateLang path = do
    let pathComponents = splitDirectories $ dropExtension path
    case pathComponents of
        ["meta", slug, langStr] -> (,,) slug Nothing <$> parseLanguage langStr
        [slugDate, langStr] ->
            case reads slugDate of
                [(day, slugDateRest)] ->
                    case slugDateRest of
                        '-':slug ->
                            (,,) slug (Just $ atMidnight day) <$>
                            parseLanguage langStr
                        _ ->
                            Left $
                            "Invalid article directory name format: " ++
                            slugDate
                _ ->
                    Left $ "Invalid article directory name format: " ++ slugDate
        _ ->
            Left $ "File path corresponds to neither article nor meta: " ++ path

-- Load the application state from a directory
loadFromDirectory :: FilePath -> IO (Either String AppData)
loadFromDirectory path = do
    sources <- sourcesFromDirectory path
    stringsFile <- readFileOrEmpty $ path </> "strings.yaml"
    linksFile <- readFileOrEmpty $ path </> "links.yaml"
    analyticsFile <- readFileOrEmpty $ path </> "analytics.yaml"
    servicesFile <- readFileOrEmpty $ path </> "services.yaml"
    crossPostFile <- readFileOrEmpty $ path </> "cross-posting.yaml"
    return $
        do (articles, metas) <- fromSources sources
           strings <- decodeOrDefault stringsFile
           links <- decodeOrDefault linksFile
           analytics <- decodeOrDefault analyticsFile
           services <- decodeOrDefault servicesFile
           crossPost <- decodeOrDefault crossPostFile
           return $
               def
               { appDirectory = path
               , appAddress = ""
               , appArticles = articles
               , appMeta = metas
               , appStrings = strings
               , appLinks = links
               , appAnalytics = analytics
               , appServices = services
               , appCrossPost = crossPost
               }

-- Group sources by slug/date and make Articles or Metas out of them
fromSources :: [ContentSource] -> Either String ([Article], [Meta])
fromSources css = do
    cs <- mapM extractContent css
    let grouped = map (foldr1 mergeContent) $ groupBy coKey cs
    let parsed = map parseContent grouped
    return (lefts parsed, rights parsed)
  where
    coKey c = (coSlug c, coDate c)

-- All content sources from a directory
sourcesFromDirectory :: FilePath -> IO [ContentSource]
sourcesFromDirectory root = execWriterT $ sourcesFromDirectory' root root

-- Read all content sources from a specified directory, considering the given
-- root
sourcesFromDirectory' :: FilePath -> FilePath -> WriterT [ContentSource] IO ()
sourcesFromDirectory' root d = do
    isDir <- liftIO $ doesDirectoryExist d
    when isDir $
        do files <- liftIO $ getDirectoryContents d
           let toTraverse = map (d </>) $ filter (not . isSpecial) files
           mapM_ (sourcesFromDirectory' root) toTraverse
    isFile <- liftIO $ doesFileExist d
    when isFile $ sourceFromFile root d

-- Read a content source from a file, considering the given root
sourceFromFile :: FilePath -> FilePath -> WriterT [ContentSource] IO ()
sourceFromFile root fp =
    case takeExtension fp of
        "" -> return ()
        '.':ext ->
            case M.lookup ext readers of
                Nothing -> return ()
                Just reader -> do
                    content <- liftM reader $ liftIO $ readFile fp
                    case content of
                        Left err ->
                            fail $ "Error reading " ++ fp ++ ": " ++ show err
                        Right pandoc ->
                            tell [ContentSource (makeRelative root fp) pandoc]
        ext -> fail $ "Invalid extension " ++ ext

-- A map of supported file formats and corresponding Pandoc readers
readers :: M.Map String (String -> Either PandocError Pandoc)
readers = M.fromList [("md", readMarkdown def)]

readFileOrEmpty :: String -> IO (B.ByteString)
readFileOrEmpty path = do
    exists <- doesFileExist path
    if exists
        then B.readFile path
        else (pure "")

-- Decode YAML, returning a default value on empty content (empty files are not
-- valid YAML)
decodeOrDefault :: (Default a, Y.FromJSON a) => B.ByteString -> Either String a
decodeOrDefault "" = Right def
decodeOrDefault s = Y.decodeEither s
