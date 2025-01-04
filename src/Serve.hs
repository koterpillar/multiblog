{-|
Action to serve the blog.
-}
module Serve where

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

import           Happstack.Server             hiding (bindPort)

import           Network.Socket

import           System.Environment

import           App

bindPort :: Int -> ResourceT IO Socket
bindPort portNumber =
    fmap snd $
    flip allocate close $ do
        let hints =
                defaultHints
                    { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
                    , addrSocketType = Stream
                    }
        addr:_ <- getAddrInfo (Just hints) (Just "::") (Just $ show portNumber)
        sock <-
            socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        bind sock (addrAddress addr)
        listen sock 5
        pure sock

-- Serve the site contents, handling SIGHUP
serve :: IO ()
serve =
    runResourceT $ do
        lport <- liftIO listenPort
    -- Manually bind the socket to close it on exception
        sock <- bindPort lport
        liftIO $ putStrLn $ "Serving on port " ++ show lport ++ "."
        let conf = nullConf {port = lport}
        cache <- liftIO initAppCache
        app <- liftIO loadAppDefault
        liftIO $ simpleHTTPWithSocket' (runApp cache app) sock conf site

listenPort :: IO Int
listenPort = maybe 8000 read <$> lookupEnv "PORT"
