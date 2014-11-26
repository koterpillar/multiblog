module Language where

import Control.Monad

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe


type Language = String

type LanguagePreference = M.Map Language Float

defaultLanguage :: Language
defaultLanguage = "en"

rankLanguage :: Language -> LanguagePreference -> Float
rankLanguage lang = fromMaybe 0 . M.lookup lang

matchLanguage :: LanguagePreference -> M.Map Language a -> Maybe a
matchLanguage = matchLanguageFunc (const 1)

matchLanguageFunc :: (a -> Float) -> LanguagePreference -> M.Map Language a -> Maybe a
matchLanguageFunc quality pref values = liftM fst $ M.maxView ranked
    where ranked = M.fromList $ M.elems $ M.mapWithKey rank values
          rank lang value = (rankLanguage lang pref * quality value, value)

-- TODO: Parsec or library
languageHeader :: Maybe String -> LanguagePreference
languageHeader Nothing = M.singleton defaultLanguage 1
languageHeader (Just str) = M.fromList $ map parsePref $ splitOn "," str
    where parsePref pref = case splitOn ";q=" pref of
                               [lang] -> (lang, 1)
                               [lang, qvalue] -> (lang, read qvalue)
