{-|
Types for the blog content - articles and metas.
-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Content where

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import Data.Time
import Data.Yaml

import Text.Pandoc hiding (Meta, readers)
import Text.Pandoc.Walk

import Types.Language

type LanguageContent = LanguageMap Pandoc

type LanguageString = LanguageMap String

class HasSlug a  where
    getSlug :: a -> String

-- TODO: lens
class HasContent a  where
    getContent :: a -> LanguageContent
    modifyContent :: (LanguageContent -> LanguageContent) -> a -> a

-- TODO: Should Article and Meta actually be one type?
data Article = Article
    { arSlug :: String
    , arContent :: LanguageContent
    , arAuthored :: UTCTime
    } deriving (Eq, Show)

instance Ord Article where
    a `compare` b = (arAuthored a, arSlug a) `compare` (arAuthored b, arSlug b)

instance HasContent Article where
    getContent = arContent
    modifyContent f a =
        a
        { arContent = f $ arContent a
        }

instance HasSlug Article where
    getSlug = arSlug

data Layout
    = BaseLayout
    | TalkLayout
    deriving (Eq, Show)

data Meta = Meta
    { mtSlug :: String
    , mtLayout :: Layout
    , mtContent :: LanguageContent
    } deriving (Eq, Show)

instance HasContent Meta where
    getContent = mtContent
    modifyContent f m =
        m
        { mtContent = f $ mtContent m
        }

instance HasSlug Meta where
    getSlug = mtSlug

data Link
    = MetaLink { lnName :: String}
    | ExternalLink { lnUrl :: String
                   , lnText :: LanguageString}
    deriving (Eq, Show)

instance FromJSON Link where
    parseJSON (Object v) =
        MetaLink <$> v .: "page" <|> do
            url <- v .: "url"
            LanguageChoices text <- v .: "text"
            return $ ExternalLink url text
    parseJSON _ = mzero

byDate :: Day -> Article -> Bool
byDate d = (== d) . utctDay . arAuthored

byYear :: Integer -> Article -> Bool
byYear y a = y == y'
  where
    (y', _, _) = toGregorian $ utctDay $ arAuthored a

byYearMonth :: Integer -> Int -> Article -> Bool
byYearMonth y m a = y == y' && m == m'
  where
    (y', m', _) = toGregorian $ utctDay $ arAuthored a

bySlug
    :: HasSlug a
    => String -> a -> Bool
bySlug slug = (== slug) . getSlug

byDateSlug :: Day -> String -> Article -> Bool
byDateSlug d s a = byDate d a && bySlug s a

-- TODO: Might be a better way to do this in Pandoc
inlineToStr :: [Inline] -> String
inlineToStr inline = writePlain def $ Pandoc undefined [Plain inline]

stripTitle :: Pandoc -> Pandoc
stripTitle (Pandoc meta blocks) = Pandoc meta blocks'
  where
    blocks' = catMaybes $ evalState (mapM stripFirst blocks) True
    stripFirst :: Block -> State Bool (Maybe Block)
    stripFirst block = do
        isFirst <- get
        if isFirst
            then case block of
                     Header _ _ _ -> do
                         put False
                         return Nothing
                     _ -> return $ Just block
            else return $ Just block

langContent
    :: HasContent a
    => LanguagePreference -> a -> Pandoc
langContent lang = fromJust . matchLanguage lang . getContent

langTitle
    :: HasContent a
    => LanguagePreference -> a -> String
langTitle lang =
    fromMaybe "untitled" . listToMaybe . query extractTitle . langContent lang
  where
    extractTitle (Header _ _ title) = [inlineToStr title]
    extractTitle _ = []
