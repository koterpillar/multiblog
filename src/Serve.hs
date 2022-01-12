{-|
Action to serve the blog.
-}
module Serve where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State

import           Happstack.Server     hiding (bindPort)

import           Network.Socket

import           System.Environment

import           App

bindPort :: Int -> IO Socket
bindPort portNumber = do
    let hints =
            defaultHints
                { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
                , addrSocketType = Stream
                }
    addr:_ <- getAddrInfo (Just hints) (Just "::") (Just $ show portNumber)
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
