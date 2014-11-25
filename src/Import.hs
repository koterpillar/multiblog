module Import where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import qualified Data.Map as M
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time

import Text.Pandoc hiding (readers)

import System.Directory
import System.FilePath.Posix

import Models


data ArticleSource = ArticleSource { asPath :: FilePath
                                   , asContent :: Pandoc
                                   }

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(v, "")] -> return v
    _ -> fail "No parse"

readDate :: String -> Maybe UTCTime
readDate = liftM atMidnight . readM

filenameData :: FilePath -> Maybe (UTCTime, String)
filenameData fn = case splitOn "-" fn of
    [ys, ms, ds, slug] -> do
        date <- readDate $ intercalate "-" [ys, ms, ds]
        return (date, slug)
    _ -> fail "No parse"

chunks :: FilePath -> [String]
chunks = reverse . dropFirstExtension . reverse . splitDirectories
    where dropFirstExtension [] = []
          dropFirstExtension (x:xs) = dropExtension x:xs

metaText :: MetaValue -> Maybe String
metaText (MetaInlines [Str text]) = Just text
metaText (MetaString text) = Just text
metaText _ = Nothing

metaValues :: String -> Bool -> ArticleSource -> [String]
metaValues name matchPath source = maybeToList (lookupMeta name m >>= metaText) ++ fpChunks
    where (Pandoc m _) = asContent source
          fpChunks = if matchPath then chunks $ asPath source else []

stringMeta :: String -> ArticleSource -> Maybe String
stringMeta name = listToMaybe . metaValues name False

dateMeta :: ArticleSource -> Maybe UTCTime
dateMeta s = msum $ readDate <$> metaValues "date" True s

readers :: M.Map String (String -> Pandoc)
readers = M.fromList [("md", readMarkdown def)]

fromFile :: FilePath -> WriterT [ArticleSource] IO ()
fromFile fp = case takeExtension fp of
    "" -> return ()
    '.':ext -> case M.lookup ext readers of
        Nothing -> return ()
        Just reader -> do
            content <- liftM reader $ liftIO $ readFile fp
            tell [ArticleSource fp content]

sourcesFromDirectory :: FilePath -> IO [ArticleSource]
sourcesFromDirectory = execWriterT . sourcesFromDirectory'

sourcesFromDirectory' :: FilePath -> WriterT [ArticleSource] IO ()
sourcesFromDirectory' d = do
    isDir <- liftIO $ doesDirectoryExist d
    when isDir $ do
        files <- liftIO $ getDirectoryContents d
        let toTraverse = map (d </>) $ filter (not . isSpecial) files
        mapM_ sourcesFromDirectory' toTraverse
    isFile <- liftIO $ doesFileExist d
    when isFile $ fromFile d

isSpecial "." = True
isSpecial ".." = True
isSpecial _ = False

fromDirectory :: FilePath -> IO [Article]
fromDirectory = liftM groupArticles . sourcesFromDirectory

groupArticles :: [ArticleSource] -> [Article]
groupArticles = M.elems . M.map makeArticle . groupSources

makeArticle :: [ArticleSource] -> Article
makeArticle [] = error "at least one source is required"
makeArticle ss@(s:_) = Article { arSlug = fromJust $ stringMeta "slug" s
                               , arContent = M.fromList [(fromJust $ stringMeta "lang" s, asContent s) | s <- ss]
                               , arAuthored = fromJust $ dateMeta s
                               }

groupSources :: [ArticleSource] -> M.Map (String, UTCTime) [ArticleSource]
groupSources = M.fromListWith (++) . map sourceKey
    where sourceKey s = ((fromJust $ stringMeta "slug" s, fromJust $ dateMeta s), [s])
