{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Base functions for integration tests
module Integration.Base
  ( makeRequest
  , simpleRequest
  , assertContains
  , assertContainsBefore
  , assertNotContains
  , testAddress
  , withLang
  , withLang1
  , withLangCookie
  ) where

import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Text as T

import Happstack.Server

import Test.Framework
import Test.HUnit

import App
import Types.Language

data TestRequest = TestRequest
    { trUri :: String
    , trHeaders :: M.Map String String
    , trCookies :: M.Map String String
    }

simpleRequest :: String -> TestRequest
simpleRequest uri = TestRequest uri M.empty M.empty

testAddress :: T.Text
testAddress = "http://test"

-- Make a request to the application
makeRequest :: TestRequest -> IO String
makeRequest req = do
    happstackReq <- mkRequest req
    app <- loadApp "testsuite/Integration/content" testAddress False
    cache <- initAppCache
    rsp <- runApp cache app $ simpleHTTP'' site happstackReq
    content <- responseContent rsp
    return content

assertContains
    :: (Eq a, Show a)
    => [a] -> [a] -> Assertion
assertContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " not found in:\n" ++ show haystack)
        (needle `isInfixOf` haystack)

assertNotContains
    :: (Eq a, Show a)
    => [a] -> [a] -> Assertion
assertNotContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " found in:\n" ++ show haystack)
        (not $ needle `isInfixOf` haystack)

assertContainsBefore
    :: (Eq a, Show a)
    => [a] -> [a] -> [a] -> Assertion
assertContainsBefore first second haystack =
    subAssert $
    assertBoolVerbose
        (show first ++ " does not precede " ++ show second ++ " in:\n" ++ show haystack)
        (second `isInfixOf`
         (head $ dropWhile (first `isInfixOf`) $ tails haystack))

-- Create a request with a specified URL
-- Happstack doesn't make it easy
mkRequest :: TestRequest -> IO Request
mkRequest TestRequest {..} = do
    let (rUri, rParams) = splitUriParam $ trUri
    inputsBody <- newEmptyMVar
    rBody <- newMVar (Body LB.empty)
    return
        Request
        { rqSecure = False
        , rqMethod = GET
        , rqPaths = filter (/= "") $ splitOn "/" rUri
        , rqUri = trUri
        , rqQuery = "?" ++ rParams
        , rqInputsQuery = splitParams rParams
        , rqInputsBody = inputsBody
        , rqCookies = cookies
        , rqVersion = HttpVersion 1 1
        , rqHeaders = headers
        , rqBody = rBody
        , rqPeer = ("", 0)
        }
  where
    splitUriParam :: String -> (String, String)
    splitUriParam rPath =
        case splitOn "?" rPath of
            [rUri] -> (rUri, "")
            [rUri, rParams] -> (rUri, rParams)
    splitParams :: String -> [(String, Input)]
    splitParams =
        map (mkParamTuple . splitOn "=") . filter (/= "") . splitOn "&"
    mkParamTuple :: [String] -> (String, Input)
    mkParamTuple [k, v] = (k, mkInputValue v)
    mkParamTuple [k] = (k, mkInputValue "")
    mkInputValue str =
        Input
        { inputValue = Right (LB.fromStrict $ U.fromString str)
        , inputFilename = Nothing
        , inputContentType =
            ContentType
            { ctType = "text"
            , ctSubtype = "plain"
            , ctParameters = []
            }
        }
    cookies = M.toList $ M.mapWithKey mkCookie trCookies
    headers = M.fromList $ map makeHeader $ M.toList $ trHeaders
    makeHeader (name, value) = (name', HeaderPair name' [value'])
      where
        name' = U.fromString $ map toLower name
        value' = U.fromString value

-- Add an Accept-Language header to a request
withLang :: LanguagePreference -> TestRequest -> TestRequest
withLang lang req =
    req
    { trHeaders = newHeaders
    }
  where
    newHeaders = M.insert "Accept-Language" pref (trHeaders req)
    pref = show lang

withLang1 :: Language -> TestRequest -> TestRequest
withLang1 = withLang . singleLanguage

-- Add a language cookie to a request
withLangCookie :: Language -> TestRequest -> TestRequest
withLangCookie lang req =
    req
    { trCookies = M.insert "lang" (showLanguage lang) (trCookies req)
    }

-- Extract contents from a response
responseContent :: Response -> IO String
responseContent r@(Response _ _ _ _ _) =
    return $ U.toString $ LB.toStrict $ rsBody r
responseContent f@(SendFile _ _ _ _ _ _ _) = do
    contents <- readFile $ sfFilePath f
    let offset = fromIntegral $ sfOffset f
    let count = fromIntegral $ sfCount f
    return $ drop offset $ take count contents
