{-|
Subcommand to obtain a user's authorization for a service to cross-post.
-}
{-# LANGUAGE OverloadedStrings #-}

module Authorize where

import Control.Monad.IO.Class

import qualified Data.ByteString as B

import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import qualified Data.Yaml as Y

import System.IO (hFlush, stdout)

import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit

import App
import Types.Services

authorize :: String -> App ()
authorize service = do
    auth <- getAuthorization service
    liftIO $ do
        putStrLn ""
        Text.putStr $ Text.decodeUtf8 $ Y.encode auth

getAuthorization :: String -> App AppAuth
getAuthorization "twitter" = authorizeTwitter
getAuthorization _ = error "Invalid service name"

authorizeTwitter :: App AppAuth
authorizeTwitter =
    withTwitter $ \auth ->
        liftIO $ do
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
    Text.encodeUtf8 <$> Text.getLine
