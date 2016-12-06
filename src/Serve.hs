module Serve where

import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Happstack.Server

import Network.Socket

import System.Environment

import App

-- Serve the site contents, handling SIGHUP
serve :: App ()
serve = do
    lport <- liftIO listenPort
    let conf =
            nullConf
            { port = lport
            }
    app <- ask
    cache <- get
    -- Manually bind the socket to close it on exception
    liftIO $ bracket
        (bindPort conf)
        close
        (\sock -> simpleHTTPWithSocket' (runApp cache app) sock conf site)

listenPort :: IO Int
listenPort = liftM (read . fromMaybe "8000") (lookupEnv "LISTEN_PORT")
