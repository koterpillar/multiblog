{-# LANGUAGE TemplateHaskell #-}

module Arbitrary where

import Prelude hiding (LT)

import Data.DeriveTH
import Data.LanguageCodes
import qualified Data.Map as M
import Data.Time

import Text.Pandoc

import Test.Framework

instance Arbitrary Day where
    arbitrary = fromGregorian <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> (mkDiffTime <$> arbitrary)
      where
        mkDiffTime = secondsToDiffTime . (`mod` 86400)

derive makeArbitrary ''Pandoc

derive makeArbitrary ''Block

derive makeArbitrary ''Meta

derive makeArbitrary ''MetaValue

derive makeArbitrary ''Alignment

derive makeArbitrary ''Format

derive makeArbitrary ''Inline

derive makeArbitrary ''ListNumberDelim

derive makeArbitrary ''ListNumberStyle

derive makeArbitrary ''Citation

derive makeArbitrary ''MathType

derive makeArbitrary ''CitationMode

derive makeArbitrary ''QuoteType

derive makeArbitrary ''ISO639_1
