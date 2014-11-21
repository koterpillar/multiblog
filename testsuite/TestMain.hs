{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestModels

main = htfMain htf_importedTests
