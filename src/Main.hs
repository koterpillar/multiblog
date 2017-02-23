{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Options.Generic
import Options.Generic.Default

import App
import Authorize
import CrossPost
import ReloadHup
import Serve

data Args
    = Authorize String
    | CrossPost
    | Serve
    deriving (Generic)

instance ParseRecord Args

main :: IO ()
main =
    reloadHup $ do
        app <- loadAppDefault
        cache <- initAppCache
        runApp cache app $ do
            args <- liftIO $ getRecordDefault Serve "Multiblog"
            case args of
                Authorize service -> authorize service
                CrossPost -> crossPost
                Serve -> serve
