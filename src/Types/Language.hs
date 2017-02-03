{-|
Language-related types.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Language where

import Control.Monad

import Data.Function
import Data.LanguageCodes
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Yaml as Y

import Utils

type Language = ISO639_1

type LanguageMap = M.Map Language

mapKeysM
    :: (Monad m, Ord k2)
    => (k1 -> m k2) -> M.Map k1 a -> m (M.Map k2 a)
mapKeysM kfunc = liftM M.fromList . mapM kvfunc . M.toList
  where
    kvfunc (k, v) = liftM (\k' -> (k', v)) $ kfunc k

instance Y.FromJSON ISO639_1 where
    parseJSON v@(Y.String _) = Y.parseJSON v >>= parseLanguageM
    parseJSON _ = mzero

instance (Y.FromJSON v) =>
         Y.FromJSON (M.Map Language v) where
    parseJSON v@(Y.Object _) = Y.parseJSON v >>= mapKeysM parseLanguageM
    parseJSON v@(Y.String _) = M.singleton defaultLanguage <$> Y.parseJSON v
    parseJSON _ = mzero

newtype LanguagePreference = LanguagePreference
    { unLanguagePreference :: LanguageMap Float
    } deriving (Eq)

defaultLanguage :: Language
defaultLanguage = EN

singleLanguage :: Language -> LanguagePreference
singleLanguage lang = LanguagePreference $ M.singleton lang 1

bestLanguage :: LanguagePreference -> Language
bestLanguage =
    fromMaybe defaultLanguage .
    listToMaybe . fmap fst . sortBy (reverseCompare `on` snd) . M.toList . unLanguagePreference

rankLanguage :: Language -> LanguagePreference -> Float
rankLanguage lang = fromMaybe 0 . M.lookup lang . unLanguagePreference

matchLanguage :: LanguagePreference -> LanguageMap a -> Maybe a
matchLanguage = matchLanguageFunc (const 1)

matchLanguageFunc :: (a -> Float)
                  -> LanguagePreference
                  -> LanguageMap a
                  -> Maybe a
matchLanguageFunc quality pref values = liftM fst $ M.maxView ranked
  where
    ranked = M.fromList $ M.elems $ M.mapWithKey rank values
    rank lang value = (rankLanguage lang pref * quality value, value)

parseLanguage :: String -> Either String Language
parseLanguage langStr =
    case parseLanguageM langStr :: Maybe Language of
        (Just lang) -> pure lang
        Nothing -> Left $ langStr ++ " is not a valid language code."

parseLanguageM
    :: MonadPlus m
    => String -> m Language
parseLanguageM [c1, c2] =
    case fromChars c1 c2 of
        Just lang -> return lang
        Nothing -> mzero
parseLanguageM (c1:c2:'-':_) = parseLanguageM [c1, c2]
parseLanguageM _ = mzero

showLanguage :: Language -> String
showLanguage = (\(a, b) -> a : b : []) . toChars

iso3166 :: Language -> String
iso3166 EN = "gb"
iso3166 ZH = "cn"
iso3166 x = showLanguage x

-- TODO: Parsec or library
languageHeader :: Maybe String -> LanguagePreference
languageHeader Nothing = LanguagePreference $ M.singleton defaultLanguage 1
languageHeader (Just str) =
    LanguagePreference $ M.fromList $ mapMaybe parsePref $ splitOn "," str
  where
    parsePref pref =
        case splitOn ";q=" pref of
            [lang] -> (pairWith 1) <$> parseLanguageM lang
            [lang, qvalue] -> (pairWith (read qvalue)) <$> parseLanguageM lang
            _ -> Nothing
    pairWith y x = (x, y)

instance Show LanguagePreference where
    show = intercalate "," . map (uncurry showPref) . M.toList . unLanguagePreference
      where
        showPref lang 1 = showLanguage lang
        showPref lang qvalue = showLanguage lang ++ ";q=" ++ show qvalue
