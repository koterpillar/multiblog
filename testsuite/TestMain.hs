{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Integration.TestBase
import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage
import {-@ HTF_TESTS @-} TestUtils
import {-@ HTF_TESTS @-} TestViews

main = htfMain htf_importedTests
