{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           Test.Framework

import           Integration.TestArticle
import           Integration.TestFeed
import           Integration.TestHome
import           Integration.TestMeta
import           Integration.TestStatic
import           TestCache
import           TestImport
import           TestLanguage
import           TestModels
import           TestRoutes
import           TestViews
import           TestViewsExport

main = htfMain htf_importedTests
