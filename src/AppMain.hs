module AppMain
    ( multiblog
    ) where

import           ReloadHup
import           Serve

multiblog :: IO ()
multiblog = reloadHup serve
