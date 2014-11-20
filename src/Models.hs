module Models where

import Control.Monad
import Control.Monad.Writer

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time

import Text.Pandoc

import System.Directory
import System.FilePath.Posix


data Article = Article { arSlug :: String
                       , arContent :: Pandoc
                       , arAuthored :: UTCTime
                       }

byDate :: Day -> Article -> Bool
byDate d = (== d) . utctDay . arAuthored

byYear :: Integer -> Article -> Bool
byYear y a = y == y'
    where (y', _, _) = toGregorian $ utctDay $ arAuthored a

byYearMonth :: Integer -> Int -> Article -> Bool
byYearMonth y m a = y == y' && m == m'
    where (y', m', _) = toGregorian $ utctDay $ arAuthored a

bySlug :: String -> Article -> Bool
bySlug slug = (== slug) . arSlug

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(v, "")] -> return v
    _ -> fail "No parse"

readDate :: String -> Maybe UTCTime
readDate s = readM s >>= \date -> Just $ UTCTime date 0

filenameData :: FilePath -> Maybe (UTCTime, String)
filenameData fn = case splitOn "-" fn of
    [ys, ms, ds, title] -> do
        date <- readDate $ intercalate "-" [ys, ms, ds]
        return (date, title)
    _ -> fail "No parse"

maybeSum :: a -> [Maybe a] -> a
maybeSum v = fromMaybe v . msum

fromPandoc :: FilePath -> Pandoc -> Article
fromPandoc fn p = Article { arSlug = maybeSum ""
                                [pandocSlug m, filenameSlug fn]
                          , arContent = p
                          , arAuthored = maybeSum (error "No date")
                                [pandocDate m, filenameDate fn]
                          }
    where (Pandoc m _) = p
          pandocSlug m = lookupMeta "slug" m >>= metaText
          pandocDate m = lookupMeta "date" m >>= metaText >>= readDate
          filenameDate = liftM fst . filenameData
          filenameSlug = liftM snd . filenameData
          metaText (MetaInlines [Str text]) = Just text
          metaText (MetaString text) = Just text
          metaText _ = Nothing

fromFile :: FilePath -> IO Article
fromFile fp = liftM (fromPandoc fn . readMarkdown def) $ readFile fp
    where fn = takeFileName fp

fromDirectory :: FilePath -> IO [Article]
fromDirectory = execWriterT . fromDirectory'

fromDirectory' :: FilePath -> WriterT [Article] IO ()
fromDirectory' d = do
    isDir <- liftIO $ doesDirectoryExist d
    when isDir $ do
        files <- liftIO $ getDirectoryContents d
        let toTraverse = map (d </>) $ filter (not . isSpecial) files
        mapM_ fromDirectory' toTraverse
    isFile <- liftIO $ doesFileExist d
    when isFile $ do
        article <- liftIO $ fromFile d
        tell [article]

isSpecial "." = True
isSpecial ".." = True
isSpecial _ = False
