-- Base functions for integration tests
module Integration.Base where

import           Control.Concurrent.MVar

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LB
import           Data.Char
import           Data.Function           (on)
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text

import           Happstack.Server

import           Test.HUnit
import           Test.Hspec.Expectations

import           App
import           Types.Language

data TestRequest =
    TestRequest
        { trUri     :: Text
        , trHeaders :: Map Text Text
        , trCookies :: Map Text Text
        }

simpleRequest :: Text -> TestRequest
simpleRequest uri = TestRequest uri Map.empty Map.empty

testAddress :: Text
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
makeRequestText = fmap (Text.decodeUtf8 . LB.toStrict) . makeRequestBS

shouldContainText :: HasCallStack => Text -> Text -> Expectation
shouldContainText = shouldContain `on` Text.unpack

shouldNotContainText :: HasCallStack => Text -> Text -> Expectation
shouldNotContainText = shouldNotContain `on` Text.unpack

assertContainsBefore ::
       (HasCallStack, Eq a, Show a) => [a] -> [a] -> [a] -> Assertion
assertContainsBefore first second haystack =
    assertBool
        (show first ++
         " does not precede " ++ show second ++ " in:\n" ++ show haystack)
        (second `isInfixOf`
         head (dropWhile (first `isInfixOf`) $ tails haystack))

assertTextContainsBefore :: HasCallStack => Text -> Text -> Text -> Assertion
assertTextContainsBefore first second haystack =
    assertContainsBefore
        (Text.unpack first)
        (Text.unpack second)
        (Text.unpack haystack)

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
            , rqPaths = map Text.unpack $ filter (/= "") $ Text.splitOn "/" rUri
            , rqUri = Text.unpack trUri
            , rqQuery = Text.unpack $ "?" <> rParams
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
        case Text.splitOn "?" rPath of
            [rUri]          -> (rUri, "")
            [rUri, rParams] -> (rUri, rParams)
            _               -> error "path should have 1 or 0 '?'"
    splitParams :: Text -> [(String, Input)]
    splitParams =
        map (mkParamTuple . Text.splitOn "=") .
        filter (/= "") . Text.splitOn "&"
    mkParamTuple :: [Text] -> (String, Input)
    mkParamTuple [k, v] = (Text.unpack k, mkInputValue v)
    mkParamTuple [k] = (Text.unpack k, mkInputValue "")
    mkParamTuple _ = error "mkParamTuple should have 1 or 2 element list input"
    mkInputValue str =
        Input
            { inputValue = Right (LB.fromStrict $ Text.encodeUtf8 str)
            , inputFilename = Nothing
            , inputContentType =
                  ContentType
                      {ctType = "text", ctSubtype = "plain", ctParameters = []}
            }
    cookies =
        Map.toList $
        Map.mapWithKey mkCookie $
        Map.mapKeys Text.unpack $ Map.map Text.unpack trCookies
    headers =
        Map.fromList $
        map makeHeader $
        Map.toList $ Map.mapKeys Text.unpack $ Map.map Text.unpack trHeaders
    makeHeader (name, value) = (name', HeaderPair name' [value'])
      where
        name' = Text.encodeUtf8 $ Text.pack $ map toLower name
        value' = Text.encodeUtf8 $ Text.pack value

-- Add an Accept-Language header to a request
withLang :: LanguagePreference -> TestRequest -> TestRequest
withLang lang req = req {trHeaders = newHeaders}
  where
    newHeaders = Map.insert "Accept-Language" pref (trHeaders req)
    pref = Text.pack $ show lang

withLang1 :: Language -> TestRequest -> TestRequest
withLang1 = withLang . singleLanguage

-- Add a language cookie to a request
withLangCookie :: Language -> TestRequest -> TestRequest
withLangCookie lang req =
    req {trCookies = Map.insert "lang" (showLanguage lang) (trCookies req)}

-- Extract contents from a response
responseContent :: Response -> IO LB.ByteString
responseContent r@Response {} = pure $ rsBody r
responseContent f@SendFile {} = do
    contents <- LB.readFile $ sfFilePath f
    let offset = fromIntegral $ sfOffset f
    let count = fromIntegral $ sfCount f
    pure $ LB.drop offset $ LB.take count contents

responseHeader :: String -> Response -> Maybe ByteString
responseHeader = getHeader
