module Main where

import Control.Monad.IO.Class

import System.Environment

import App
import Authorize
import CrossPost
import ReloadHup
import Serve

main :: IO ()
main =
    reloadHup $
    do app <- loadAppDefault
       cache <- initAppCache
       runApp cache app $
           do args <- liftIO getArgs
              case args of
                  ["authorize", service] -> authorize service
                  ["cross-post"] -> crossPost
                  [] -> serve
                  _ -> error "Invalid usage"
