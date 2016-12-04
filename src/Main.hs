module Main where

import System.Environment

import Authorize
import CrossPost
import Serve

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["authorize", service] -> authorize service
        ["cross-post"] -> crossPost
        [] -> serve
        _ -> error "Invalid usage"
