{-# LANGUAGE FlexibleContexts #-}
-- Import the application data (articles and meta) from a set of files.
module Import where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.ByteString.UTF8 as U
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


-- Each file gets imported into a ContentSource
data ContentSource = ContentSource { csPath    :: FilePath
                                   , csContent :: Pandoc
                                   }
    deriving (Eq, Ord, Show)

-- Each language group of ContentSources becomes either Article or Meta
-- later
data Content = Content { coSlug    :: String
                       , coDate    :: Maybe UTCTime
                       , coContent :: LanguageContent
                       }
    deriving (Eq, Ord, Show)

parseContent :: Content -> Either Article Meta
parseContent c = case coDate c of
    Just dt -> Left $ Article (coSlug c) (coContent c) dt
    Nothing -> Right $ Meta (coSlug c) (coContent c)

addContent :: [Content] -> State AppState ()
addContent = mapM_ $ \c -> modify $ \st -> case parseContent c of
    -- TODO: this greatly benefits from Lenses on AppState
    Left a  -> st { appArticles = appArticles st ++ [a] }
    Right m -> st { appMeta = appMeta st ++ [m] }

-- A meta value extracted from a content source. Some (e.g. from the
-- Pandoc meta) will have a name associated, some will not.
data MetaInfo = Named { miName :: String, miValue :: String }
              | Unnamed { miValue :: String }
    deriving (Eq, Show)

metaValues :: ContentSource -> [MetaInfo]
metaValues cs = filenameInfo (csPath cs) ++ pandocInfo (csContent cs)

-- Split file path into MetaInfos
-- For example, "some/directory/file.md" will be split into unnamed strings:
-- "some", "directory", "file"
filenameInfo :: FilePath -> [MetaInfo]
filenameInfo = map Unnamed . reverse . dropFirstExtension . reverse . splitDirectories
    where dropFirstExtension [] = []
          dropFirstExtension (x:xs) = dropExtension x:xs

-- Read MetaInfos from Pandoc
pandocInfo :: Pandoc -> [MetaInfo]
pandocInfo (Pandoc meta _) = catMaybes $ map mkInfo $ M.toList $ unMeta meta
    where mkInfo (name, value) = do
              text <- metaText value
              return $ Named name text
          metaText (MetaInlines [Str text]) = Just text
          metaText (MetaString text) = Just text
          metaText _ = Nothing

extractContent :: ContentSource -> Either String Content
extractContent cs = flip evalStateT (metaValues cs) ecst
    where ecst :: StateT [MetaInfo] (Either String) Content
          ecst = do
                lang' <- liftM (require "Language is required") $ extractLanguage
                lang <- lift lang'
                date <- extractDate
                slug' <- liftM (require "Slug is required") $ extractSlug
                slug <- lift slug'
                return $ Content slug date $ M.singleton lang $ csContent cs

fromSources :: [ContentSource] -> Either String ([Article], [Meta])
fromSources = undefined

-- MetaInfo sets can be used to extract typed information
-- Extracting something changes the set

-- Try to read the language off the end of a string
extractLanguage :: MonadState [MetaInfo] s => s (Maybe Language)
extractLanguage = extractFromMeta "language" parseLang
    where split3 :: String -> (String, String)
          split3 (x:y:z:t:u) = ((x:hd), tl) where (hd, tl) = split3 (y:z:t:u)
          split3 tl = ("", tl)
          parseLang :: String -> Maybe (Language, String)
          parseLang str = let (hd, tl) = split3 str in case tl of
              '-':langStr -> case parseLanguage langStr of
                                 Just lang -> Just (lang, hd)
                                 _ -> Nothing
              _ -> Nothing

-- Try to read the date off the start
extractDate :: MonadState [MetaInfo] s => s (Maybe UTCTime)
extractDate = extractFromMeta "date" $ \str -> case reads str of
    [(dt, rest)] -> Just (atMidnight dt, rest)
    _ -> Nothing

-- Extract the slug, this can be any string
extractSlug :: MonadState [MetaInfo] s => s (Maybe String)
extractSlug = extractFromMeta "slug" $ \str -> Just (str, "")

-- Extract either unnamed or named meta, applying a parser afterwards
extractFromMeta :: MonadState [MetaInfo] s => String -> (String -> Maybe (a, String)) -> s (Maybe a)
extractFromMeta name parser = extractIf test
    where test (Named name' value) | name == name' = liftM (second (Named name)) $ parser value
                                   | otherwise = Nothing
          test (Unnamed value) = liftM (second Unnamed) $ parser value

-- Extract a meta matching a function
extractIf :: MonadState [c] s => (c -> Maybe (a, c)) -> s (Maybe a)
extractIf test = do
        st <- get
        let (result, st') = tryExtract st
        put st'
        return result
    where tryExtract [] = (Nothing, [])
          tryExtract (m:ms) = case test m of
              Just (result, m') -> (Just result, m':ms)
              Nothing -> (result, m:ms') where (result, ms') = tryExtract ms

-- Load the application state from a directory
loadFromDirectory :: FilePath -> IO (Either String AppState)
loadFromDirectory path = do
    sources <- sourcesFromDirectory path
    stringsFile <- readFile $ path </> "strings.yaml"
    return $ do
        (articles, metas) <- fromSources sources
        strings <- loadStrings stringsFile
        return $ emptyState { appDirectory = path
                            , appAddress = ""
                            , appArticles = articles
                            , appMeta = metas
                            , appStrings = strings
                            }

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

-- A map of supported file formats and corresponding Pandoc readers
readers :: M.Map String (String -> Pandoc)
readers = M.fromList [("md", readMarkdown def)]

-- Load translations from a YAML file
loadStrings :: String -> Either String (M.Map String (LanguageMap String))
loadStrings = Y.decodeEither . U.fromString
