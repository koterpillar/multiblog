{-# LANGUAGE DeriveDataTypeable #-}

module ReloadHup
    ( reloadHup
    , Reload
    ) where

import           Control.Concurrent
import           Control.Exception

import           Data.Typeable

import           System.Environment
import           System.Posix.Process
import           System.Posix.Signals

data Reload = Reload
    deriving (Show, Typeable)

instance Exception Reload

reloadHup :: IO () -> IO ()
reloadHup main = do
    mainThread <- myThreadId
    _ <-
        installHandler
            lostConnection -- SIGHUP
            (CatchOnce $ reloadExecutable mainThread)
            Nothing
    main

-- Replace the process with a (possibly updated) executable
-- Throw a "Reload" exception to the main thread so it releases
-- its socket first
reloadExecutable :: ThreadId -> IO ()
reloadExecutable mainThread = do
    throwTo mainThread Reload
    ownPath <- getExecutablePath
    executeFile ownPath False [] Nothing
