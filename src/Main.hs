{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Data.Maybe
import qualified Data.Text as T

import Happstack.Server

import System.Environment

import Web.Routes.Happstack

import App

siteAddress :: IO String
siteAddress = do
    addr <- lookupEnv "SITE_URL"
    return $ fromMaybe "http://localhost:8000" addr

main :: IO ()
main = do
    address <- siteAddress
    listenPort <- lookupEnv "LISTEN_PORT"
    let conf = nullConf { port = read $ fromMaybe "8000" listenPort }
    simpleHTTP' runApp conf $ implSite (T.pack address) "" site
