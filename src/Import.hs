{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Import the application data (articles and meta) from a set of files.
module Import where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.ByteString.UTF8 as U
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import qualified Data.Yaml as Y

import Text.Pandoc hiding (Meta, readers)
import Text.Pandoc.Error

import System.Directory
import System.FilePath.Posix

import Language
import Models
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

-- Merge content with same date/slug
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

-- A meta value extracted from a content source. Some (e.g. from the
-- Pandoc meta) will have a name associated, some will not.
data MetaInfo
    = Named { miName :: String
            , miValue :: String}
    | Unnamed { miValue :: String}
    deriving (Eq, Show)

metaValues :: ContentSource -> [MetaInfo]
metaValues cs = pandocInfo (csContent cs) ++ filenameInfo (csPath cs)

-- Split file path into MetaInfos
-- For example, "some/directory/file.md" will be split into unnamed strings:
-- "file", "directory", "some"
-- Strings are in reverse order because the file name is the most specific
filenameInfo :: FilePath -> [MetaInfo]
filenameInfo = map Unnamed . dropFirstExtension . reverse . splitDirectories
  where
    dropFirstExtension [] = []
    dropFirstExtension (x:xs) = dropExtension x : xs

-- Read MetaInfos from Pandoc
pandocInfo :: Pandoc -> [MetaInfo]
pandocInfo (Pandoc meta _) = catMaybes $ map mkInfo $ M.toList $ unMeta meta
  where
    mkInfo (name, value) = do
        text <- metaText value
        return $ Named name text
    metaText (MetaInlines [Str text]) = Just text
    metaText (MetaString text) = Just text
    metaText _ = Nothing

extractContent :: ContentSource -> Either String Content
extractContent cs =
    flip evalStateT (metaValues cs) $
    do lang' <- liftM (require "Language is required") $ extractLanguage
       lang <- lift lang'
       date <- extractDate
       slug' <- liftM (require "Slug is required") $ extractSlug
       slug <- lift slug'
       return $ Content slug date $ M.singleton lang $ csContent cs

-- MetaInfo sets can be used to extract typed information
-- Extracting something changes the set
-- Extracting a value from a source consumes all or part of the source
type Extractor source value = source -> Maybe (value, Maybe source)

liftExtracted
    :: (source -> source')
    -> Maybe (value, Maybe source)
    -> Maybe (value, Maybe source')
liftExtracted = liftM . second . liftM

-- Try to read the language off the end of a string
extractLanguage
    :: MonadState [MetaInfo] s
    => s (Maybe Language)
extractLanguage = extractFromMeta "lang" parseLang
  where
    split3 :: [a] -> ([a], [a])
    split3 (x:xs@(_:_:_:_)) = ((x : hd), tl)
      where
        (hd, tl) = split3 xs
    split3 xs = ([], xs)
    parseLang :: Extractor String Language
    parseLang str =
        let (hd, tl) = split3 str
        in case tl of
               '-':langStr ->
                   case parseLanguage langStr of
                       Just lang -> Just (lang, notEmpty hd)
                       _ -> Nothing
               langStr ->
                   case parseLanguage langStr of
                       Just lang -> Just (lang, Nothing)
                       _ -> Nothing

-- Try to read the date off the start
extractDate
    :: MonadState [MetaInfo] s
    => s (Maybe UTCTime)
extractDate =
    extractFromMeta "date" $
    \str ->
         case reads str of
             [(dt, rest)] -> Just (atMidnight dt, notEmpty rest)
             _ -> Nothing

-- Treat empty lists/strings as Nothing
notEmpty :: [a] -> Maybe [a]
notEmpty [] = Nothing
notEmpty s = Just s

-- Extract the slug, this can be any string
extractSlug
    :: MonadState [MetaInfo] s
    => s (Maybe String)
extractSlug = extractFromMeta "slug" $ \str -> Just (str, Nothing)

-- Extract either unnamed or named meta, applying a parser afterwards
extractFromMeta
    :: MonadState [MetaInfo] s
    => String -> Extractor String a -> s (Maybe a)
extractFromMeta name parser = extractIf test
  where
    test (Named name' value)
        | name == name' = liftExtracted (Named name) $ parser value
        | otherwise = Nothing
    test (Unnamed value) = liftExtracted Unnamed $ parser value

-- Extract a meta matching a function
extractIf
    :: MonadState [c] s
    => Extractor c a -> s (Maybe a)
extractIf test = do
    st <- get
    let (result, st') = tryExtract st
    put st'
    return result
  where
    tryExtract [] = (Nothing, [])
    tryExtract (m:ms) =
        case test m of
            Just (result, Nothing) -> (Just result, ms)
            Just (result, Just m') -> (Just result, m' : ms)
            Nothing -> (result, m : ms')
                where (result, ms') = tryExtract ms

-- Load the application state from a directory
loadFromDirectory :: FilePath -> IO (Either String AppData)
loadFromDirectory path = do
    sources <- sourcesFromDirectory path
    stringsFile <- readFile $ path </> "strings.yaml"
    linksFile <- readFile $ path </> "links.yaml"
    analyticsFile <- readFile $ path </> "analytics.yaml"
    return $
        do (articles, metas) <- fromSources sources
           strings <- loadStrings stringsFile
           links <- loadLinks linksFile
           analytics <- loadAnalytics analyticsFile
           return $
               emptyState
               { appDirectory = path
               , appAddress = ""
               , appArticles = articles
               , appMeta = metas
               , appStrings = strings
               , appLinks = links
               , appAnalytics = analytics
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
sourcesFromDirectory = execWriterT . sourcesFromDirectory'

-- Read all content sources from a directory
sourcesFromDirectory' :: FilePath -> WriterT [ContentSource] IO ()
sourcesFromDirectory' d = do
    isDir <- liftIO $ doesDirectoryExist d
    when isDir $
        do files <- liftIO $ getDirectoryContents d
           let toTraverse = map (d </>) $ filter (not . isSpecial) files
           mapM_ sourcesFromDirectory' toTraverse
    isFile <- liftIO $ doesFileExist d
    when isFile $ sourceFromFile d

-- Read a content source from a file
sourceFromFile :: FilePath -> WriterT [ContentSource] IO ()
sourceFromFile fp =
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
                        Right pandoc -> tell [ContentSource fp pandoc]
        ext -> fail $ "Invalid extension " ++ ext

-- A map of supported file formats and corresponding Pandoc readers
readers :: M.Map String (String -> Either PandocError Pandoc)
readers = M.fromList [("md", readMarkdown def)]

-- Load translations from a YAML file
loadStrings :: String -> Either String (M.Map String LanguageString)
loadStrings = Y.decodeEither . U.fromString

loadLinks :: String -> Either String [Link]
loadLinks = Y.decodeEither . U.fromString

-- Load analytics keys
loadAnalytics :: String -> Either String Analytics
loadAnalytics = Y.decodeEither . U.fromString
