{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Control.Monad

import Data.Maybe
import qualified Data.Text as T

import Filesystem.Path.CurrentOS

import Happstack.Server

import System.Argv0
import System.Environment
import System.Posix.Process
import System.Posix.Signals

import Web.Routes.Happstack

import App

-- Run the site, handling SIGHUP
main :: IO ()
main = do
    _ <- installHandler lostConnection (CatchOnce reloadExecutable) Nothing
    runSite

-- Replace the process with a (possibly updated) executable
reloadExecutable :: IO ()
reloadExecutable = do
    ownPath <- liftM encodeString getArgv0
    -- TODO: Need to close the listening socket before executing again
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
    simpleHTTP' runApp conf $ implSite (T.pack address) "" site
