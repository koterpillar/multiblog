module Models where

import Control.Monad
import Control.Monad.Writer

import Data.Maybe
import Data.Time

import Text.Pandoc

import System.Directory
import System.FilePath.Posix


data Article = Article { arTitle :: String
                       , arContent :: Content
                       , arAuthored :: UTCTime
                       }

type Content = Pandoc

byDate :: Day -> Article -> Bool
byDate d = (== d) . utctDay . arAuthored

byYear :: Integer -> Article -> Bool
byYear y a = y == y'
    where (y', _, _) = toGregorian $ utctDay $ arAuthored a

byYearMonth :: Integer -> Int -> Article -> Bool
byYearMonth y m a = y == y' && m == m'
    where (y', m', _) = toGregorian $ utctDay $ arAuthored a

bySlug :: String -> Article -> Bool
bySlug slug = (== slug) . slugify . arTitle

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a

-- TODO
slugify :: String -> String
slugify = id

fromPandoc :: Pandoc -> Article
fromPandoc p = Article { arTitle = pandocTitle p
                       , arContent = p
                       -- TODO
                       , arAuthored = UTCTime (fromGregorian 2014 11 08) 0
                       }
    where pandocTitle (Pandoc _ blocks) = fromMaybe "" $
              msum $ map getHeader blocks
          getHeader (Header _ _ [Str text]) = Just text
          getHeader _ = Nothing


fromFile :: FilePath -> IO Article
fromFile = liftM (fromPandoc . readMarkdown def) . readFile

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
