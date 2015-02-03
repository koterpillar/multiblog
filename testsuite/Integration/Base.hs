{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

-- Base functions for integration tests
module Integration.Base where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U
import Data.List
import Data.List.Split
import qualified Data.Map as M

import Happstack.Server

import Test.Framework
import Test.HUnit

import App
import Import
import Language


-- Site handler with a test address
testHandler :: ServerPartT App Response
testHandler = site "http://test"

-- Make a request to the application
testRequest :: Request -> IO Response
testRequest req = do
    app <- loadApp "testsuite/Integration/content"
    runApp app $ simpleHTTP'' testHandler req

assertContains :: (Eq a, Show a) => [a] -> [a] -> Assertion
assertContains needle haystack =
    subAssert $ assertBoolVerbose
        (show needle ++ " not found in:\n" ++ show haystack)
        (needle `isInfixOf` haystack)

-- Create a request with a specified URL
-- Happstack doesn't make it easy
mkRequest :: String -> IO Request
mkRequest rPath = do
    inputsBody <- newEmptyMVar
    rBody <- newMVar (Body LB.empty)
    return Request { rqSecure = False
                   , rqMethod = GET
                   , rqPaths = filter (/= "") $ splitOn "/" rPath
                   , rqUri = rPath
                   , rqQuery = ""
                   , rqInputsQuery = []
                   , rqInputsBody = inputsBody
                   , rqCookies = []
                   , rqVersion = HttpVersion 1 1
                   , rqHeaders = M.empty
                   , rqBody = rBody
                   , rqPeer = ("", 0)
                   }

withLang :: LanguagePreference -> Request -> Request
withLang lang req = req { rqHeaders = newHeaders }
    where newHeaders = M.insert "accept-language" (HeaderPair "Accept-Language" [U.fromString pref]) (rqHeaders req)
          pref = show lang

withLang1 :: Language -> Request -> Request
withLang1 lang = withLang $ singleLanguage lang

-- Extract contents from a response
responseContent :: Response -> IO String
responseContent r@(Response _ _ _ _ _) = return $ U.toString $ LB.toStrict $ rsBody r
responseContent f@(SendFile _ _ _ _ _ _ _) = do
    contents <- readFile $ sfFilePath f
    let offset = fromIntegral $ sfOffset f
    let count = fromIntegral $ sfCount f
    return $ drop offset $ take count contents
