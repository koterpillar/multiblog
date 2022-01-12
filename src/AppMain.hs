{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AppMain
    ( multiblog
    ) where

import           App
import           ReloadHup
import           Serve

multiblog :: IO ()
multiblog =
    reloadHup $ do
        app <- loadAppDefault
        cache <- initAppCache
        runApp cache app serve
