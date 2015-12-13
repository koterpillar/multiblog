{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} Integration.TestArticle
import {-@ HTF_TESTS @-} Integration.TestHome
import {-@ HTF_TESTS @-} Integration.TestMeta
import {-@ HTF_TESTS @-} Integration.TestRSS
import {-@ HTF_TESTS @-} Integration.TestStatic
import {-@ HTF_TESTS @-} TestCache
import {-@ HTF_TESTS @-} TestImport
import {-@ HTF_TESTS @-} TestLanguage
import {-@ HTF_TESTS @-} TestModels
import {-@ HTF_TESTS @-} TestRoutes
import {-@ HTF_TESTS @-} TestUtils
import {-@ HTF_TESTS @-} TestViews

main = htfMain htf_importedTests
