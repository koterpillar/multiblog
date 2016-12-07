{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import Prelude hiding (LT)

import Data.DeriveTH
import Data.LanguageCodes

import Text.Pandoc

import Test.Framework

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
