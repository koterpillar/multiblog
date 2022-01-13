module Integration.TestStatic where

import           Integration.Base
import           Test.HUnit

unit_static :: IO ()
unit_static = do
    resp <- makeRequestBS $ simpleRequest "/some-verification-file.html"
    assertEqual "" "This is the exact content of the verification file.\n" resp

unit_static_index :: IO ()
unit_static_index = do
    resp <- makeRequestBS $ simpleRequest "/with_index/"
    assertEqual "" "Index file\n" resp
