{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- Import the application data (articles and meta) from a set of files.
module Import where

import           Control.Lens          ((&))
import           Control.Monad
import           Control.Monad.Except

import           Data.Aeson
import qualified Data.ByteString       as B
import           Data.Default.Class
import           Data.List             (find)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Time
import qualified Data.Yaml             as Yaml

import           Text.Pandoc           hiding (Meta, readers)

import           System.Directory
import           System.FilePath.Posix

import           Models
import           Types.Content
import           Types.Language

-- | A file read from the content directory
data SourceFile =
    SourceFile
        { sfName    :: Text -- ^ File name
        , sfContent :: B.ByteString -- ^ File contents
        }
    deriving (Eq, Ord, Show)

-- | Read directory with a list of files
data SourceDirectory =
    SourceDirectory
        { sdName  :: Text -- ^ Directory name
        , sdFiles :: [SourceFile] -- ^ Files inside
        }
    deriving (Show)

-- | Get file contents by name from a directory
sdFile :: Text -> SourceDirectory -> Maybe B.ByteString
sdFile name = fmap sfContent . find (\f -> sfName f == name) . sdFiles

type ParseT m a = ExceptT String m a

data MetaOptions =
    MetaOptions
        { moLayout     :: Layout
        , moExportSlug :: Maybe Text
        }

instance Default MetaOptions where
    def = MetaOptions {moLayout = BaseLayout, moExportSlug = Nothing}

instance FromJSON MetaOptions where
    parseJSON (Object v) = do
        layout <- fromMaybe BaseLayout <$> v .:? "layout"
        exportSlug <- v .:? "exportSlug"
        return MetaOptions {moLayout = layout, moExportSlug = exportSlug}
    parseJSON _ = mzero

-- | Parse meta from a directory
parseMeta :: Monad m => SourceDirectory -> ParseT m Meta
parseMeta dir = do
    content <- parseContent dir
    options <- decodeOrDefault $ sdFile "options.yaml" dir
    pure
        Meta
            { mtSlug = sdName dir
            , mtLayout = moLayout options
            , mtExportSlugOverride = moExportSlug options
            , mtContent = content
            }

-- | Parse an article from a directory
parseArticle :: Monad m => SourceDirectory -> ParseT m Article
parseArticle dir = do
    (slug, date) <- extractSlugDate $ sdName dir
    content <- parseContent dir
    pure Article {arSlug = slug, arAuthored = date, arContent = content}

-- | Parse a multilingual content from a directory
parseContent :: Monad m => SourceDirectory -> ParseT m LanguageContent
parseContent dir = do
    content <-
        fmap catMaybes $
        forM (sdFiles dir) $ \file -> do
            let (fileName, ext) = splitExtension $ Text.unpack $ sfName file
            case Map.lookup (Text.pack $ tail ext) readers of
                Nothing -> pure Nothing
                Just reader -> do
                    lang <- parseLanguage (Text.pack fileName)
                    case reader (Text.decodeUtf8 $ sfContent file) of
                        Left err  -> throwError $ show err
                        Right res -> pure (Just (lang, res))
    pure $ Map.fromList content

invalidArticleDirectory :: Monad m => Text -> ParseT m a
invalidArticleDirectory dir =
    throwError $ Text.unpack $ "Invalid article directory name format: " <> dir

invalidFilePath :: Monad m => FilePath -> ParseT m a
invalidFilePath path =
    throwError $ "File path corresponds to neither article nor meta: " ++ path

-- | Get slug and date from a directory path
extractSlugDate :: Monad m => Text -> ParseT m (Text, UTCTime)
extractSlugDate name =
    case reads (Text.unpack name) of
        [(day, slugDateRest)] ->
            case slugDateRest of
                '-':slug -> pure (Text.pack slug, atMidnight day)
                _        -> invalidArticleDirectory name
        _ -> invalidArticleDirectory name

-- | Parse all articles and metas from the contents directory
parseTree ::
       FilePath -- ^ Root directory
    -> ParseT IO ([Article], [Meta])
parseTree root = do
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
buildDirs ::
       MonadIO m
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
    wantDir subdir = notElem subdir ignore && head subdir /= '.'
    dirPath subdir = dir </> subdir

-- | Enumerate files in a directory
buildDir ::
       MonadIO m
    => FilePath -- ^ Directory to traverse
    -> m SourceDirectory
buildDir dir =
    liftIO $ do
        contents <- listDirectory dir
        files <- filterM doesFileExist $ map dirPath contents
        sources <-
            forM files $ \file -> do
                content <- B.readFile file
                pure
                    SourceFile
                        { sfName = Text.pack $ takeFileName file
                        , sfContent = content
                        }
        pure
            SourceDirectory
                {sdName = Text.pack $ takeFileName dir, sdFiles = sources}
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
    settings <- decodeOrDefault $ rootFile "settings.yaml"
    pure $
        def
            { appDirectory = path
            , appAddress = ""
            , appArticles = articles
            , appMeta = metas
            , appStrings = strings
            , appLinks = links
            , appSettings = settings
            }

-- A map of supported file formats and corresponding Pandoc readers
readers :: Map Text (Text -> Either PandocError Pandoc)
readers =
    Map.fromList
        [ ( "md"
          , runPandocPure .
            readMarkdown
                def
                    { readerExtensions =
                          readerExtensions def &
                          enableExtension Ext_backtick_code_blocks &
                          enableExtension Ext_pipe_tables
                    })
        ]

readFileOrEmpty :: String -> IO B.ByteString
readFileOrEmpty path = do
    exists <- doesFileExist path
    if exists
        then B.readFile path
        else pure ""

-- Decode YAML, returning a default value on empty content (empty files are not
-- valid YAML)
decodeOrDefault ::
       (Default a, FromJSON a, Monad m) => Maybe B.ByteString -> ParseT m a
decodeOrDefault Nothing = pure def
decodeOrDefault (Just s) =
    case Yaml.decodeEither' s of
        Left err  -> throwError $ show err
        Right res -> pure res
