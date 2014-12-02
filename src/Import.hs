module Import where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Time

import Text.Pandoc hiding (readers)

import System.Directory
import System.FilePath.Posix

import Models


data ArticleSource = ArticleSource { asPath    :: FilePath
                                   , asContent :: Pandoc
                                   }

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(v, _)] -> return v
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

fromDirectory :: FilePath -> IO (Either String [Article])
fromDirectory = liftM groupArticles . sourcesFromDirectory

mapAllRight :: M.Map k (Either e v) -> Either e (M.Map k v)
mapAllRight m = let (bad, good) = M.mapEither id m in case M.toList bad of
    [] -> Right good
    (_, err):_ -> Left err

groupArticles :: [ArticleSource] -> Either String [Article]
groupArticles = liftM M.elems . mapAllRight . M.map makeArticle . groupSources

-- TODO: better name
mfes :: ArticleSource -> String -> Maybe a -> Either String a
mfes as err = mfe (err ++ " in " ++ asPath as)

mfe :: String -> Maybe a -> Either String a
mfe _ (Just v) = Right v
mfe err Nothing = Left err

makeArticle :: [ArticleSource] -> Either String Article
makeArticle [] = fail "at least one source is required"
makeArticle ss@(s:_) = do
    slug <- mfes s "Slug is required" $ stringMeta "slug" s
    date <- mfes s "Date is required" $ dateMeta s
    content <- liftM M.fromList $ forM ss $ \s -> do
        lang <- mfes s "Language is required" $ stringMeta "lang" s
        let pandoc = asContent s
        return (lang, pandoc)
    Right Article { arSlug = slug
                  , arContent = content
                  , arAuthored = date
                  }

groupSources :: [ArticleSource] -> M.Map (String, UTCTime) [ArticleSource]
groupSources = M.fromListWith (++) . map sourceKey
    where sourceKey s = ((fromJust $ stringMeta "slug" s, fromJust $ dateMeta s), [s])
