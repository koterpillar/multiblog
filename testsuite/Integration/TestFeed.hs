module Integration.TestFeed where

import           Data.Default.Class
import           Data.Foldable

import           Data.XML.Types

import           Text.Atom.Feed
import           Text.Atom.Feed.Import   (elementFeed)
import           Text.Atom.Feed.Validate

import qualified Text.XML                as C

import           Integration.Base
import           Test.HUnit
import           Test.Hspec

atomEntry :: Name
atomEntry = "{http://www.w3.org/2005/Atom}entry"

unit_home :: IO ()
unit_home = do
    rss <- makeRequestBS $ simpleRequest "/feed/en"
    xml <-
        case C.parseLBS def rss of
            Left exc  -> error $ show exc
            Right res -> pure res
    let root = documentRoot $ C.toXMLDocument xml
    -- Make sure every entry validates
    let entryElements = elementChildren root >>= isNamed atomEntry
    entryElements `shouldSatisfy` (not . null)
    forM_ entryElements $ \entryElement ->
        assertEqual "" [] $ flattenT $ validateEntry entryElement
    -- Check feed contents
    let Just feed = elementFeed root
    assertEqual "" "Test site" $ txtToString $ feedTitle feed
    -- TODO: Web.Routes generate a link without the trailing slash
    assertEqual "" (testAddress <> "/") $ feedId feed
    assertEqual "" "2018-01-01T00:00:00Z" $ feedUpdated feed
    let entries = feedEntries feed
    assertEqual "" ["Статья с кодом", "Another article"] $
        map (txtToString . entryTitle) $ take 2 entries
    let entry2 = entries !! 1
    fmap personName (entryAuthors entry2) `shouldBe` ["Author Name"]
    let Just (HTMLContent content) = entryContent entry2
    -- FIXME: convert the content to XML representation properly
    assertEqual
        ""
        "<p>This article should appear above the first one.</p>"
        content
