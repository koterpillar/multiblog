{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.LanguageCodes.Arbitrary where

import Prelude hiding (LT)

import Data.DeriveTH
import Data.LanguageCodes (ISO639_1(..))

import Test.QuickCheck

derive makeArbitrary ''ISO639_1
