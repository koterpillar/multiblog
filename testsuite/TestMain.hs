{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Integration.TestArticle
import {-@ HTF_TESTS @-} Integration.TestHome
import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage
import {-@ HTF_TESTS @-} TestUtils
import {-@ HTF_TESTS @-} TestViews

main = htfMain htf_importedTests
