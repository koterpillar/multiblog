{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Options.Generic
import Options.Generic.Default

import App
import Authorize
import ReloadHup
import Serve

data Args
    = Authorize String
    | Main
    deriving (Generic)

instance ParseRecord Args

main :: IO ()
main =
    reloadHup $ do
        app <- loadAppDefault
        cache <- initAppCache
        runApp cache app $ do
            args <- liftIO $ getRecordDefault Main "Multiblog"
            case args of
                Authorize service -> authorize service
                Main -> crossPostAndServe
