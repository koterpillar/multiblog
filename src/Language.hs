{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language where

import Control.Applicative
import Control.Monad

import Data.LanguageCodes
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Yaml as Y


type Language = ISO639_1

type LanguageMap = M.Map Language

mapKeysM :: (Monad m, Ord k1, Ord k2) => (k1 -> m k2) -> M.Map k1 a -> m (M.Map k2 a)
mapKeysM kfunc = liftM M.fromList . mapM kvfunc . M.toList
    where kvfunc (k, v) = liftM (\k' -> (k', v)) $ kfunc k

instance (Y.FromJSON v) => Y.FromJSON (M.Map Language v) where
    parseJSON v = Y.parseJSON v >>= mapKeysM parseLanguage

newtype LanguagePreference = LanguagePreference { unLanguagePreference :: LanguageMap Float }
    deriving (Eq)

defaultLanguage :: Language
defaultLanguage = EN

rankLanguage :: Language -> LanguagePreference -> Float
rankLanguage lang = fromMaybe 0 . M.lookup lang . unLanguagePreference

matchLanguage :: LanguagePreference -> LanguageMap a -> Maybe a
matchLanguage = matchLanguageFunc (const 1)

matchLanguageFunc :: (a -> Float) -> LanguagePreference -> LanguageMap a -> Maybe a
matchLanguageFunc quality pref values = liftM fst $ M.maxView ranked
    where ranked = M.fromList $ M.elems $ M.mapWithKey rank values
          rank lang value = (rankLanguage lang pref * quality value, value)

parseLanguage :: MonadPlus m => String -> m Language
parseLanguage [c1, c2] = case fromChars c1 c2 of
                             Just lang -> return lang
                             Nothing -> mzero
parseLanguage _ = mzero

-- TODO: Parsec or library
languageHeader :: Maybe String -> LanguagePreference
languageHeader Nothing = LanguagePreference $ M.singleton defaultLanguage 1
languageHeader (Just str) = LanguagePreference $ M.fromList $ mapMaybe parsePref $ splitOn "," str
    where parsePref pref = case splitOn ";q=" pref of
                               [lang] -> (pairWith 1) <$> parseLanguage lang
                               [lang, qvalue] -> (pairWith (read qvalue)) <$> parseLanguage lang
                               _ -> Nothing
          pairWith y x = (x, y)

instance Show LanguagePreference where
    show = intercalate "," . map (uncurry showPref) . M.toList . unLanguagePreference
        where showPref lang 1 = languageStr lang
              showPref lang qvalue = languageStr lang ++ ";q=" ++ show qvalue
              languageStr = (\(a, b) -> a:b:[]) . toChars
