module Serve where

import Control.Exception
import Control.Monad

import Data.Maybe

import Happstack.Server

import Network.Socket

import System.Environment

import App
import ReloadHup

-- Serve the site contents, handling SIGHUP
serve :: IO ()
serve =
    reloadHup $
    do address <- siteAddress
       -- TODO: directory name as parameter?
       app <- loadApp "content" address
       cache <- initAppCache
       lport <- listenPort
       let conf =
               nullConf
               { port = lport
               }
       -- Manually bind the socket to close it on exception
       bracket
           (bindPort conf)
           close
           (\sock -> simpleHTTPWithSocket' (runApp cache app) sock conf site)

siteAddress :: IO String
siteAddress = do
    addr <- lookupEnv "SITE_URL"
    return $ fromMaybe "http://localhost:8000" addr

listenPort :: IO Int
listenPort = liftM (read . fromMaybe "8000") (lookupEnv "LISTEN_PORT")
