{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators      #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Maybe
import Data.Typeable

import Filesystem.Path.CurrentOS

import Happstack.Server

import Network.Socket

import System.Argv0
import System.Environment
import System.Posix.Process
import System.Posix.Signals

import App

data Reload = Reload
    deriving (Show, Typeable)

instance Exception Reload

-- Run the site, handling SIGHUP
main :: IO ()
main = do
    mainThread <- myThreadId
    _ <- installHandler lostConnection
        (CatchOnce $ reloadExecutable mainThread) Nothing
    runSite

-- Replace the process with a (possibly updated) executable
-- Throw a "Reload" exception to the main thread so it releases
-- its socket first
reloadExecutable :: ThreadId -> IO ()
reloadExecutable mainThread = do
    throwTo mainThread Reload
    ownPath <- liftM encodeString getArgv0
    executeFile ownPath False [] Nothing

siteAddress :: IO String
siteAddress = do
    addr <- lookupEnv "SITE_URL"
    return $ fromMaybe "http://localhost:8000" addr

listenPort :: IO Int
listenPort = liftM (read . fromMaybe "8000") (lookupEnv "LISTEN_PORT")

-- Run the actual site
runSite :: IO ()
runSite = do
    address <- siteAddress
    lport <- listenPort
    let conf = nullConf { port = lport }
    -- Manually bind the socket to close it on exception
    bracket
        (bindPort conf)
        close
        (\sock -> simpleHTTPWithSocket' runApp sock conf $ siteHandler address)
