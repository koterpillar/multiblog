{-|
Action to serve the blog.
-}
module Serve where

import Control.Concurrent.Lifted
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Happstack.Server

import Network.Socket

import System.Environment

import App
import CrossPost
import Models

crossPostAndServe :: App ()
crossPostAndServe = do
    address <- asks appAddress
    case address of
        ImplicitSiteAddress _ -> pure ()
        ExplicitSiteAddress _ -> void $ fork crossPost
    serve

-- Serve the site contents, handling SIGHUP
serve :: App ()
serve = do
    lport <- liftIO listenPort
    liftIO $ putStrLn $ "Serving on port " ++ show lport ++ "."
    let conf = nullConf {port = lport}
    app <- ask
    cache <- get
    -- Manually bind the socket to close it on exception
    liftIO $
        bracket
            (bindPort conf)
            close
            (\sock -> simpleHTTPWithSocket' (runApp cache app) sock conf site)

listenPort :: IO Int
listenPort = maybe 8000 read <$> lookupEnv "LISTEN_PORT"
