{-# LANGUAGE OverloadedStrings #-}

module Authorize where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.Yaml as Y

import System.IO (hFlush, stdout)

import Web.Twitter.Conduit hiding (map)
import Web.Authenticate.OAuth as OA

import App
import Models
import Types.Services

authorize :: String -> IO ()
authorize service = do
    auth <- getAuthorization service
    putStrLn ""
    putStr $ U.toString $ Y.encode auth

getAuthorization :: String -> IO AppAuth
getAuthorization "twitter" = authorizeTwitter
getAuthorization _ = error "Invalid service name"

withTwitter :: AppData -> (OAuth -> IO a) -> IO a
withTwitter app act =
    case asTwitter (appServices app) of
        Nothing -> error "Twitter credentials not defined"
        Just auth -> act auth

authorizeTwitter :: IO AppAuth
authorizeTwitter = do
    app <- loadAppDefault
    withTwitter app $ \auth -> do
        mgr <- newManager tlsManagerSettings
        tempCred <- OA.getTemporaryCredential auth mgr
        let url = OA.authorizeUrl auth tempCred
        pin <- getPIN url
        cred <-
            OA.getAccessToken
                auth
                (OA.insert "oauth_verifier" pin tempCred)
                mgr
        return $ toAppAuth $ twitterAuth cred

getPIN :: String -> IO B.ByteString
getPIN url = do
    putStrLn $ "Open the following URL: " ++ url
    putStr "PIN: "
    hFlush stdout
    fmap U.fromString getLine
