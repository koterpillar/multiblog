{-|
Action to serve the blog.
-}
module Serve where

import           Control.Concurrent.Lifted
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Happstack.Server hiding (bindPort)

import           Network.Socket

import           System.Environment

import           App
import           CrossPost
import           Models

crossPostAndServe :: App ()
crossPostAndServe = do
    isRealAddress <- asks appRealAddress
    when isRealAddress $ void $ fork crossPost
    serve

bindPort :: Int -> IO Socket
bindPort port = do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just "::") (Just $ show port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 5
    pure sock

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
            (bindPort lport)
            close
            (\sock -> simpleHTTPWithSocket' (runApp cache app) sock conf site)

listenPort :: IO Int
listenPort = maybe 8000 read <$> lookupEnv "PORT"
