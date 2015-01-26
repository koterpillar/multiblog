-- Import the application data (articles and meta) from a set of files.
module Import where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import qualified Data.ByteString.UTF8 as U
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import qualified Data.Yaml as Y

import Text.Pandoc hiding (Meta, readers)

import System.Directory
import System.FilePath.Posix

import Language
import Models
import Utils


data ContentSource = ContentSource { csPath    :: FilePath
                                   , csContent :: Pandoc
                                   }
    deriving (Eq, Ord, Show)

-- Parse a date from string
readDate :: String -> Maybe UTCTime
readDate = liftM atMidnight . readM

-- Extract a date and a slug from a filename
filenameData :: FilePath -> Maybe (UTCTime, String)
filenameData fn = case splitOn "-" fn of
    [ys, ms, ds, slug] -> do
        date <- readDate $ intercalate "-" [ys, ms, ds]
        return (date, slug)
    _ -> fail "No parse"

-- Split file path into chunks which data can be extracted from
-- For example, "some/directory/file.md" will be split into
-- ["some", "directory", "file"]
chunks :: FilePath -> [String]
chunks = reverse . dropFirstExtension . reverse . splitDirectories
    where dropFirstExtension [] = []
          dropFirstExtension (x:xs) = dropExtension x:xs

-- Extract a text value from Pandoc Meta
metaText :: MetaValue -> Maybe String
metaText (MetaInlines [Str text]) = Just text
metaText (MetaString text) = Just text
metaText _ = Nothing

-- Extract all meta values of a specific name from a content source
metaValues :: String -- The meta attribute name to extract
           -> Bool -- Whether to also extract from the file path
           -> ContentSource -- Content source
           -> [String] -- Extracted list of values
metaValues name matchPath source = maybeToList (lookupMeta name m >>= metaText) ++ fpChunks
    where (Pandoc m _) = csContent source
          fpChunks = if matchPath then chunks $ csPath source else []

-- Extract a value of a specific type from a content source
metaValue :: String -- The meta attribute name to extract
          -> Bool -- Whether to also extract from the file path
          -> (String -> Maybe a) -- A function to parse a value
          -> ContentSource -- Content source
          -> Maybe a -- Extracted value
metaValue name matchPath readFunc source =
    msum $ readFunc <$> metaValues name matchPath source

-- Extract a string attribute from a content source
stringMeta :: String -- Attribute name
           -> ContentSource -- Content source
           -> Maybe String -- Attribute value
stringMeta name = listToMaybe . metaValues name False

-- Extract a language from a content source
langMeta :: ContentSource -> Maybe Language
langMeta = metaValue "lang" False parseLanguage

-- Extract a date attribute from a content source
-- The file path chunks will be considered as well
dateMeta :: ContentSource -> Maybe UTCTime
dateMeta = metaValue "date" True readDate

-- A map of supported file formats and corresponding Pandoc readers
readers :: M.Map String (String -> Pandoc)
readers = M.fromList [("md", readMarkdown def)]

-- Load the application state from a directory
loadFromDirectory :: FilePath -> IO (Either String AppState)
loadFromDirectory path = do
    sources <- sourcesFromDirectory path
    stringsFile <- readFile $ path </> "strings.yaml"
    return $ do
        state <- fromSources sources
        strings <- loadStrings stringsFile
        return $ state { appDirectory = path, appStrings = strings }

-- All content sources from a directory
sourcesFromDirectory :: FilePath -> IO [ContentSource]
sourcesFromDirectory = execWriterT . sourcesFromDirectory'

-- Read all content sources from a directory
sourcesFromDirectory' :: FilePath -> WriterT [ContentSource] IO ()
sourcesFromDirectory' d = do
    isDir <- liftIO $ doesDirectoryExist d
    when isDir $ do
        files <- liftIO $ getDirectoryContents d
        let toTraverse = map (d </>) $ filter (not . isSpecial) files
        mapM_ sourcesFromDirectory' toTraverse
    isFile <- liftIO $ doesFileExist d
    when isFile $ sourceFromFile d

-- Read a content source from a file
sourceFromFile :: FilePath -> WriterT [ContentSource] IO ()
sourceFromFile fp = case takeExtension fp of
    "" -> return ()
    '.':ext -> case M.lookup ext readers of
        Nothing -> return ()
        Just reader -> do
            content <- liftM reader $ liftIO $ readFile fp
            tell [ContentSource fp content]
    ext -> fail $ "Invalid extension " ++ ext

fromSources :: [ContentSource] -> Either String AppState
fromSources sources = do
    let grouped = M.elems $ groupSources sources
    extracted <- mapM makeArticle grouped
    let (articles, meta) = partitionEithers extracted
    return AppState { appDirectory = ""
                    , appArticles = articles
                    , appMeta = meta
                    , appStrings = M.empty
                    }

-- Convert Maybe to Either, appending a content source's file name
-- to an error message
-- TODO: better name
mfes :: ContentSource -> String -> Maybe a -> Either String a
mfes as err = mfe (err ++ " in " ++ csPath as)

-- Merge content sources into an article
makeArticle :: [ContentSource] -> Either String (Either Article Meta)
makeArticle [] = fail "at least one source is required"
makeArticle ss@(s1:_) = do
    content <- mergeLanguageContent ss
    slug <- mfes s1 "Slug is required" $ stringMeta "slug" s1
    return $ case dateMeta s1 of
        Just dt -> Left Article { arSlug = slug
                                , arContent = content
                                , arAuthored = dt
                                }
        Nothing -> Right Meta { mtSlug = slug
                              , mtContent = content
                              }

-- Merge language content from a group of sources
mergeLanguageContent :: [ContentSource] -> Either String LanguageContent
mergeLanguageContent ss = liftM M.fromList $ forM ss $ \s -> do
    lang <- mfes s "Language is required" $ langMeta s
    let pandoc = csContent s
    return (lang, pandoc)

-- Group items from content sources
groupSources :: [ContentSource] -> M.Map ContentSourceIndex [ContentSource]
groupSources = M.fromListWith (++) . map sourceKeyVal
    where sourceKeyVal s = (sourceKey s, [s])

data ContentSourceIndex = ArticleContentSource String UTCTime
                        | MetaContentSource String
    deriving (Eq, Ord)

-- Whether two content sources belong to the same item (article/meta)
sourceKey :: ContentSource -> ContentSourceIndex
sourceKey s = case dateMeta s of
                  Just dt -> ArticleContentSource slug dt
                  Nothing -> MetaContentSource slug
    where slug = fromJust $ stringMeta "slug" s

-- Load translations from a YAML file
loadStrings :: String -> Either String (M.Map String (LanguageMap String))
loadStrings = Y.decodeEither . U.fromString
