{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage
import {-@ HTF_TESTS @-} TestUtils

main = htfMain htf_importedTests
