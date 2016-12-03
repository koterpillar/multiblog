module Main where

import System.Environment

import Authorize
import Serve

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["authorize", service] -> authorize service
        [] -> serve
        _ -> error "Invalid usage"
