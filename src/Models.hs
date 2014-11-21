module Models where

import Control.Monad
import Control.Monad.Writer

import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time

import Text.Pandoc hiding (readers)

import System.Directory
import System.FilePath.Posix


-- TODO: builtin?
type Language = String

data Article = Article { arSlug :: String
                       , arContent :: M.Map Language Pandoc
                       , arAuthored :: UTCTime
                       }

arLangContent :: Language -> Article -> Maybe Pandoc
arLangContent lang = M.lookup lang . arContent

mkDate :: Integer -> Int -> Int -> UTCTime
mkDate y m d = UTCTime (fromGregorian y m d) 0

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
    [ys, ms, ds, slug] -> do
        date <- readDate $ intercalate "-" [ys, ms, ds]
        return (date, slug)
    _ -> fail "No parse"

maybeSum :: a -> [Maybe a] -> a
maybeSum v = fromMaybe v . msum

fromPandoc :: FilePath -> Pandoc -> Article
fromPandoc fp p = Article { arSlug = maybeSum ""
                                [pandocSlug m, filenameSlug fn]
                          -- TODO
                          , arContent = M.fromList [("en", p)]
                          , arAuthored = maybeSum (error "No date")
                                [pandocDate m, filenameDate fn]
                          }
    where fn = takeFileName fp
          (Pandoc m _) = p
          pandocSlug m = lookupMeta "slug" m >>= metaText
          pandocDate m = lookupMeta "date" m >>= metaText >>= readDate
          filenameDate = liftM fst . filenameData
          filenameSlug = liftM snd . filenameData
          metaText (MetaInlines [Str text]) = Just text
          metaText (MetaString text) = Just text
          metaText _ = Nothing

readers :: M.Map String (String -> Pandoc)
readers = M.fromList [("md", readMarkdown def)]

fromFile :: FilePath -> WriterT [Article] IO ()
fromFile fp = case takeExtension fp of
    "" -> return ()
    '.':ext -> case M.lookup ext readers of
        Nothing -> return ()
        Just reader -> do
            content <- liftIO $ readFile fp
            let article = fromPandoc fp $ reader content
            tell [article]

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
    when isFile $ fromFile d

isSpecial "." = True
isSpecial ".." = True
isSpecial _ = False
