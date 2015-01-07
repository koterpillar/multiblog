{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage
import {-@ HTF_TESTS @-} TestUtils

main = htfMain htf_importedTests
