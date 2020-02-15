{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.LanguageCodes.Arbitrary where

import           Data.LanguageCodes (ISO639_1 (..), fromChars)

import           Data.Maybe

import           Test.QuickCheck

instance Arbitrary ISO639_1 where
    arbitrary = elements $ catMaybes $ fromChars <$> letter <*> letter
      where
        letter = ['a' .. 'z']
