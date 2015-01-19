{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Maybe
import qualified Data.Text as T
import Data.Typeable

import Filesystem.Path.CurrentOS

import Happstack.Server

import Network.Socket

import System.Argv0
import System.Environment
import System.Posix.Process
import System.Posix.Signals

import Web.Routes.Happstack

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
reloadExecutable :: ThreadId -> IO ()
reloadExecutable mainThread = do
    throwTo mainThread Reload
    ownPath <- liftM encodeString getArgv0
    executeFile ownPath False [] Nothing

siteAddress :: IO String
siteAddress = do
    addr <- lookupEnv "SITE_URL"
    return $ fromMaybe "http://localhost:8000" addr

runSite :: IO ()
runSite = do
    address <- siteAddress
    listenPort <- lookupEnv "LISTEN_PORT"
    let conf = nullConf { port = read $ fromMaybe "8000" listenPort }
    bracket
        (bindPort conf)
        close
        (\sock -> simpleHTTPWithSocket' runApp sock conf $ implSite (T.pack address) "" site)
