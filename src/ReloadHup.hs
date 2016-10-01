{-# LANGUAGE DeriveDataTypeable #-}

module ReloadHup
  ( reloadHup
  , Reload
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Typeable

import Filesystem.Path.CurrentOS

import System.Argv0
import System.Posix.Process
import System.Posix.Signals

data Reload =
    Reload
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
    ownPath <- liftM encodeString getArgv0
    executeFile ownPath False [] Nothing
