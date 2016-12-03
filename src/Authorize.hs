{-# LANGUAGE OverloadedStrings #-}

module Authorize where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.Yaml as Y

import System.IO (hFlush, stdout)

import Web.Twitter.Conduit
import Web.Authenticate.OAuth as OA

import App
import Models

authorize :: String -> IO ()
authorize service = do
    auth <- getAuthorization service
    putStrLn ""
    putStr $ U.toString $ Y.encode auth

getAuthorization :: String -> IO ServiceAuth
getAuthorization "twitter" = authorizeTwitter
getAuthorization _ = error "Invalid service name"

authorizeTwitter :: IO ServiceAuth
authorizeTwitter = do
    app <- loadAppDefault
    case asTwitter (appServices app) of
        Nothing -> error "Twitter credentials not defined"
        Just oauth -> do
            mgr <- newManager tlsManagerSettings
            tempCred <- OA.getTemporaryCredential oauth mgr
            let url = OA.authorizeUrl oauth tempCred
            pin <- getPIN url
            cred <-
                OA.getAccessToken
                    oauth
                    (OA.insert "oauth_verifier" pin tempCred)
                    mgr
            return $ twitterAuth cred

getPIN :: String -> IO B.ByteString
getPIN url = do
    putStrLn $ "Open the following URL: " ++ url
    putStr "PIN: "
    hFlush stdout
    fmap U.fromString getLine
