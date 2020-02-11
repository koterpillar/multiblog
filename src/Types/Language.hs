{-|
Language-related types.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Language where

import Control.Monad
import Control.Monad.Except

import qualified Data.Aeson.Types as A
import Data.Function
import Data.LanguageCodes
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Utils

type Language = ISO639_1

type LanguageMap = Map Language

mapKeysM :: (Monad m, Ord k2) => (k1 -> m k2) -> Map k1 a -> m (Map k2 a)
mapKeysM kfunc = fmap Map.fromList . traverse kvfunc . Map.toList
  where
    kvfunc (k, v) = (,) <$> kfunc k <*> pure v

instance A.FromJSON ISO639_1 where
    parseJSON v@(A.String _) = A.parseJSON v >>= parseLanguageM
    parseJSON _ = mzero

instance A.FromJSONKey ISO639_1 where
    fromJSONKey = A.FromJSONKeyTextParser $ parseLanguageM

-- | Newtype for parsing either a single value or a map of values
newtype LanguageChoices a =
    LanguageChoices (LanguageMap a)

instance A.FromJSON v => A.FromJSON (LanguageChoices v) where
    parseJSON v@(A.Object _) =
        LanguageChoices <$> (A.parseJSON v >>= mapKeysM parseLanguageM)
    parseJSON v@(A.String _) =
        LanguageChoices . Map.singleton defaultLanguage <$> A.parseJSON v
    parseJSON _ = mzero

newtype LanguagePreference = LanguagePreference
    { unLanguagePreference :: LanguageMap Float
    } deriving (Eq)

defaultLanguage :: Language
defaultLanguage = EN

singleLanguage :: Language -> LanguagePreference
singleLanguage lang = LanguagePreference $ Map.singleton lang 1

bestLanguage :: LanguagePreference -> Language
bestLanguage =
    fromMaybe defaultLanguage .
    listToMaybe .
    fmap fst .
    sortBy (reverseCompare `on` snd) . Map.toList . unLanguagePreference

rankLanguage :: Language -> LanguagePreference -> Float
rankLanguage lang = fromMaybe 0 . Map.lookup lang . unLanguagePreference

matchLanguage :: LanguagePreference -> LanguageMap a -> Maybe a
matchLanguage = matchLanguageFunc (const 1)

matchLanguageFunc ::
       (a -> Float) -> LanguagePreference -> LanguageMap a -> Maybe a
matchLanguageFunc quality pref values = fst <$> Map.maxView ranked
  where
    ranked = Map.fromList $ Map.elems $ Map.mapWithKey rank values
    rank lang value = (rankLanguage lang pref * quality value, value)

parseLanguage :: MonadError String m => Text -> m Language
parseLanguage langStr =
    case parseLanguageM langStr :: Maybe Language of
        (Just lang) -> pure lang
        Nothing -> throwError $ Text.unpack $ langStr <> " is not a valid language code."

parseLanguageM :: MonadPlus m => Text -> m Language
parseLanguageM = parseLanguageStr . Text.unpack
    where
        parseLanguageStr [c1, c2] =
            case fromChars c1 c2 of
                Just lang -> return lang
                Nothing -> mzero
        parseLanguageStr (c1:c2:'-':_) = parseLanguageStr [c1, c2]
        parseLanguageStr _ = mzero

showLanguage :: Language -> Text
showLanguage = Text.pack . (\(a, b) -> [a, b]) . toChars

iso3166 :: Language -> Text
iso3166 EN = "gb"
iso3166 ZH = "cn"
iso3166 x = showLanguage x

-- TODO: Parsec or library
languageHeader :: Maybe String -> LanguagePreference
languageHeader Nothing = LanguagePreference $ Map.singleton defaultLanguage 1
languageHeader (Just str) =
    LanguagePreference $ Map.fromList $ mapMaybe parsePref $ splitOn "," str
  where
    parsePref pref =
        case splitOn ";q=" pref of
            [lang] -> pairWith 1 <$> parseLanguageM (Text.pack lang)
            [lang, qvalue] -> pairWith (read qvalue) <$> parseLanguageM (Text.pack lang)
            _ -> Nothing
    pairWith y x = (x, y)

instance Show LanguagePreference where
    show =
        Text.unpack .
        Text.intercalate "," .
        map (uncurry showPref) . Map.toList . unLanguagePreference
      where
        showPref lang 1 = showLanguage lang
        showPref lang qvalue = showLanguage lang <> ";q=" <> Text.pack (show qvalue)
