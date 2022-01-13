{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module TestLanguage where

import           Data.LanguageCodes
import           Data.LanguageCodes.Arbitrary ()

import qualified Data.Map                     as Map

import           Types.Language

import           Test.Framework

mkPreference :: [(Language, Float)] -> LanguagePreference
mkPreference = LanguagePreference . M.fromList

test_matchLanguageFunc :: IO ()
test_matchLanguageFunc = do
    let values = M.fromList [(EN, "English"), (RU, "Russian"), (ZH, "Chinese")]
    assertEqual
        (Nothing :: Maybe String)
        (matchLanguage (mkPreference [(RU, 1)]) M.empty)
    assertEqual (Just "Russian") (matchLanguage (mkPreference [(RU, 1)]) values)

test_languageHeader :: IO ()
test_languageHeader = do
    assertEqual (languageHeader Nothing) (mkPreference [(EN, 1)])
    assertEqual (languageHeader $ Just "fr") (mkPreference [(FR, 1)])
    assertEqual
        (languageHeader $ Just "de,fr,ko")
        (mkPreference [(DE, 1), (FR, 1), (KO, 1)])
    assertEqual
        (languageHeader $ Just "ru,zh;q=0.8,en;q=0.6")
        (mkPreference [(RU, 1), (ZH, 0.8), (EN, 0.6)])

test_parseLanguage :: IO ()
test_parseLanguage = do
    assertEqual (Right EN) (parseLanguage "en")
    assertEqual (Right IT) (parseLanguage "it")
    assertEqual (Left "zz is not a valid language code.") (parseLanguage "zz")
    assertEqual (Right EN) (parseLanguage "en-AU")
    assertEqual (Right ZH) (parseLanguage "zh-Hans")

-- Test the function is total
prop_bestLanguageExists :: Maybe String -> Bool
prop_bestLanguageExists s = x == x
  where
    x = bestLanguage $ languageHeader s

instance Arbitrary LanguagePreference where
    arbitrary = mkPreference <$> arbitrary

prop_showLanguageHeader :: LanguagePreference -> Bool
prop_showLanguageHeader m = languageHeader (Just (show m)) == m

prop_mapKeysM :: [(String, String)] -> Bool
prop_mapKeysM l = mapKeysM Just m == Just m
  where
    m = M.fromList l

prop_bestLanguage :: LanguagePreference -> Bool
prop_bestLanguage lp = M.null m || all (<= bestValue) (M.elems m)
  where
    Just bestValue = M.lookup (bestLanguage lp) m
    m = unLanguagePreference lp
