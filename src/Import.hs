{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- Import the application data (articles and meta) from a set of files.
module Import where

import Control.Monad
import Control.Monad.Except

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B
import Data.Default.Class
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import qualified Data.Yaml as Y

import Text.Pandoc hiding (Meta, readers)

import System.Directory
import System.FilePath.Posix

import Models
import Types.Content
import Types.Language

-- | A file read from the content directory
data SourceFile = SourceFile
    { sfName :: FilePath -- ^ File name
    , sfContent :: B.ByteString -- ^ File contents
    } deriving (Eq, Ord, Show)

-- | Read directory with a list of files
data SourceDirectory = SourceDirectory
    { sdName :: FilePath -- ^ Directory name
    , sdFiles :: [SourceFile] -- ^ Files inside
    } deriving (Show)

-- | Get file contents by name from a directory
sdFile :: FilePath -> SourceDirectory -> Maybe B.ByteString
sdFile name =
    fmap sfContent . listToMaybe . filter (\f -> sfName f == name) . sdFiles

type ParseT m a = ExceptT String m a

data MetaOptions = MetaOptions
    { moLayout :: Layout
    }

instance Default MetaOptions where
    def = MetaOptions {moLayout = BaseLayout}

instance FromJSON MetaOptions where
    parseJSON (Object v) = do
        layout <- v .: "layout"
        return $ MetaOptions { moLayout = layout }
    parseJSON _ = mzero

-- | Parse meta from a directory
parseMeta :: Monad m => SourceDirectory -> ParseT m Meta
parseMeta dir = do
    content <- parseContent dir
    options <- decodeOrDefault $ sdFile "options.yaml" dir
    pure $
        Meta {mtSlug = sdName dir, mtLayout = moLayout options, mtContent = content}

-- | Parse an article from a directory
parseArticle
    :: Monad m
    => SourceDirectory -> ParseT m Article
parseArticle dir = do
    (slug, date) <- extractSlugDate $ sdName dir
    content <- parseContent dir
    pure $ Article {arSlug = slug, arAuthored = date, arContent = content}

-- | Parse a multilingual content from a directory
parseContent :: Monad m => SourceDirectory -> ParseT m LanguageContent
parseContent dir = do
    content <-
        fmap catMaybes $
        forM (sdFiles dir) $ \file -> do
            let (fileName, ext) = splitExtension $ sfName file
            case M.lookup (tail ext) readers of
                Nothing -> pure Nothing
                Just reader -> do
                    lang <- parseLanguage fileName
                    case reader (B.toString $ sfContent file) of
                        Left err -> throwError $ show err
                        Right res -> pure (Just (lang, res))
    pure $ M.fromList content

invalidArticleDirectory
    :: Monad m
    => FilePath -> ParseT m a
invalidArticleDirectory dir =
    throwError $ "Invalid article directory name format: " ++ dir

invalidFilePath
    :: Monad m
    => FilePath -> ParseT m a
invalidFilePath path =
    throwError $ "File path corresponds to neither article nor meta: " ++ path

-- | Get slug and date from a directory path
extractSlugDate
    :: Monad m
    => FilePath -> ParseT m (String, UTCTime)
extractSlugDate name =
    case reads name of
        [(day, slugDateRest)] ->
            case slugDateRest of
                '-':slug -> pure (slug, atMidnight day)
                _ -> invalidArticleDirectory name
        _ -> invalidArticleDirectory name

-- | Parse all articles and metas from the contents directory
parseTree
    :: FilePath -- ^ Root directory
    -> ParseT IO ([Article], [Meta])
parseTree root
 = do
    let metaDir = root </> "meta"
    metaExists <- liftIO $ doesDirectoryExist metaDir
    metas <-
        if metaExists
            then do
                metasDirs <- buildDirs metaDir []
                forM metasDirs parseMeta
            else pure []
    articleDirs <- buildDirs root ["meta", "static"]
    articles <- forM articleDirs parseArticle
    pure (articles, metas)

-- | Enumerate the directories
buildDirs
    :: MonadIO m
    => FilePath -- ^ Directory to traverse
    -> [FilePath] -- ^ Directories to ignore
    -> m [SourceDirectory]
buildDirs dir ignore =
    liftIO $ do
        contents <- listDirectory dir
        subdirs <-
            filterM doesDirectoryExist $ map dirPath $ filter wantDir contents
        forM subdirs buildDir
  where
    wantDir subdir = not (elem subdir ignore) && head subdir /= '.'
    dirPath subdir = dir </> subdir

-- | Enumerate files in a directory
buildDir
    :: MonadIO m
    => FilePath -- ^ Directory to traverse
    -> m SourceDirectory
buildDir dir =
    liftIO $ do
        contents <- listDirectory dir
        files <- filterM doesFileExist $ map dirPath contents
        sources <-
            forM files $ \file -> do
                content <- B.readFile file
                pure $
                    SourceFile {sfName = takeFileName file, sfContent = content}
        pure $ SourceDirectory {sdName = takeFileName dir, sdFiles = sources}
  where
    dirPath subdir = dir </> subdir

-- | Load the application state from a directory
loadFromDirectory :: FilePath -> ParseT IO AppData
loadFromDirectory path = do
    (articles, metas) <- parseTree path
    root <- buildDir path
    let rootFile name = sdFile name root
    strings <- decodeOrDefault $ rootFile "strings.yaml"
    links <- decodeOrDefault $ rootFile "links.yaml"
    analytics <- decodeOrDefault $ rootFile "analytics.yaml"
    services <- decodeOrDefault $ rootFile "services.yaml"
    crossPost <- decodeOrDefault $ rootFile "cross-posting.yaml"
    pure $
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
decodeOrDefault
    :: (Default a, FromJSON a, Monad m)
    => Maybe B.ByteString -> ParseT m a
decodeOrDefault Nothing = pure def
decodeOrDefault (Just s) =
    case Y.decodeEither s of
        Left err -> throwError err
        Right res -> pure res
