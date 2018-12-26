{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- Base functions for integration tests
module Integration.Base
    ( TestRequest
    , makeRequest
    , makeRequestBS
    , makeRequestText
    , simpleRequest
    , assertContains
    , assertContainsBefore
    , assertNotContains
    , assertTextContains
    , assertTextContainsBefore
    , assertTextNotContains
    , testAddress
    , withLang
    , withLang1
    , withLangCookie
    ) where

import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Happstack.Server

import Test.Framework
import Test.HUnit

import App
import Types.Language

data TestRequest = TestRequest
    { trUri :: Text
    , trHeaders :: M.Map Text Text
    , trCookies :: M.Map Text Text
    }

simpleRequest :: Text -> TestRequest
simpleRequest uri = TestRequest uri M.empty M.empty

testAddress :: T.Text
testAddress = "http://test"

-- | Make a request to the application
makeRequest :: TestRequest -> IO Response
makeRequest req = do
    happstackReq <- mkRequest req
    app <- loadApp "testsuite/Integration/content" testAddress False
    cache <- initAppCache
    runApp cache app $ simpleHTTP'' site happstackReq

makeRequestBS :: TestRequest -> IO LB.ByteString
makeRequestBS req = makeRequest req >>= responseContent

makeRequestText :: TestRequest -> IO Text
makeRequestText = fmap (T.decodeUtf8 . LB.toStrict) . makeRequestBS

assertContains :: (Eq a, Show a) => [a] -> [a] -> Assertion
assertContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " not found in:\n" ++ show haystack)
        (needle `isInfixOf` haystack)

assertNotContains :: (Eq a, Show a) => [a] -> [a] -> Assertion
assertNotContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " found in:\n" ++ show haystack)
        (not $ needle `isInfixOf` haystack)

assertTextContains :: Text -> Text -> Assertion
assertTextContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " not found in:\n" ++ show haystack)
        (needle `T.isInfixOf` haystack)

assertTextNotContains :: Text -> Text -> Assertion
assertTextNotContains needle haystack =
    subAssert $
    assertBoolVerbose
        (show needle ++ " found in:\n" ++ show haystack)
        (not $ needle `T.isInfixOf` haystack)

assertContainsBefore :: (Eq a, Show a) => [a] -> [a] -> [a] -> Assertion
assertContainsBefore first second haystack =
    subAssert $
    assertBoolVerbose
        (show first ++
         " does not precede " ++ show second ++ " in:\n" ++ show haystack)
        (second `isInfixOf`
         head (dropWhile (first `isInfixOf`) $ tails haystack))

assertTextContainsBefore :: Text -> Text -> Text -> Assertion
assertTextContainsBefore first second haystack =
    subAssert $
    assertBoolVerbose
        (show first ++
         " does not precede " ++ show second ++ " in:\n" ++ show haystack)
        (second `T.isInfixOf`
         head (dropWhile (first `T.isInfixOf`) $ T.tails haystack))

-- Create a request with a specified URL
-- Happstack doesn't make it easy
mkRequest :: TestRequest -> IO Request
mkRequest TestRequest {..} = do
    let (rUri, rParams) = splitUriParam trUri
    inputsBody <- newEmptyMVar
    rBody <- newMVar (Body LB.empty)
    return
        Request
        { rqSecure = False
        , rqMethod = GET
        , rqPaths = map T.unpack $ filter (/= "") $ T.splitOn "/" rUri
        , rqUri = T.unpack trUri
        , rqQuery = T.unpack $ "?" <> rParams
        , rqInputsQuery = splitParams rParams
        , rqInputsBody = inputsBody
        , rqCookies = cookies
        , rqVersion = HttpVersion 1 1
        , rqHeaders = headers
        , rqBody = rBody
        , rqPeer = ("", 0)
        }
  where
    splitUriParam :: Text -> (Text, Text)
    splitUriParam rPath =
        case T.splitOn "?" rPath of
            [rUri] -> (rUri, "")
            [rUri, rParams] -> (rUri, rParams)
            _ -> error "path should have 1 or 0 '?'"
    splitParams :: Text -> [(String, Input)]
    splitParams =
        map (mkParamTuple . T.splitOn "=") . filter (/= "") . T.splitOn "&"
    mkParamTuple :: [Text] -> (String, Input)
    mkParamTuple [k, v] = (T.unpack k, mkInputValue v)
    mkParamTuple [k] = (T.unpack k, mkInputValue "")
    mkParamTuple _ = error "mkParamTuple should have 1 or 2 element list input"
    mkInputValue str =
        Input
        { inputValue = Right (LB.fromStrict $ T.encodeUtf8 str)
        , inputFilename = Nothing
        , inputContentType =
              ContentType
              {ctType = "text", ctSubtype = "plain", ctParameters = []}
        }
    cookies =
        M.toList $
        M.mapWithKey mkCookie $ M.mapKeys T.unpack $ M.map T.unpack trCookies
    headers =
        M.fromList $
        map makeHeader $
        M.toList $ M.mapKeys T.unpack $ M.map T.unpack trHeaders
    makeHeader (name, value) = (name', HeaderPair name' [value'])
      where
        name' = T.encodeUtf8 $ T.pack $ map toLower name
        value' = T.encodeUtf8 $ T.pack value

-- Add an Accept-Language header to a request
withLang :: LanguagePreference -> TestRequest -> TestRequest
withLang lang req = req {trHeaders = newHeaders}
  where
    newHeaders = M.insert "Accept-Language" pref (trHeaders req)
    pref = T.pack $ show lang

withLang1 :: Language -> TestRequest -> TestRequest
withLang1 = withLang . singleLanguage

-- Add a language cookie to a request
withLangCookie :: Language -> TestRequest -> TestRequest
withLangCookie lang req =
    req
    {trCookies = M.insert "lang" (T.pack $ showLanguage lang) (trCookies req)}

-- Extract contents from a response
responseContent :: Response -> IO LB.ByteString
responseContent r@Response {} = pure $ rsBody r
responseContent f@SendFile {} = do
    contents <- LB.readFile $ sfFilePath f
    let offset = fromIntegral $ sfOffset f
    let count = fromIntegral $ sfCount f
    pure $ LB.drop offset $ LB.take count contents
