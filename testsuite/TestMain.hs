{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage

main = htfMain htf_importedTests
