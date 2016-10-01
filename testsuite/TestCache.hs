{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestCache where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State

import Cache

import Test.Framework


test_cached = do
    -- Test caching a function that depends on internal state
    values <- newMVar [1, 2, 3]
    let nextValue :: IO Int
        nextValue = modifyMVar values $ \(v:vs) -> return (vs, v)

    cache <- initCache :: IO (Cache String Int)

    -- The first call should call the function and get 1
    firstValue <- withCache cache "mykey" nextValue
    assertEqual 1 firstValue

    -- Same cache key, this should return 1
    cachedValue <- withCache cache "mykey" nextValue
    assertEqual 1 cachedValue

    -- Different cache key
    anotherValue <- withCache cache "anotherkey" nextValue
    assertEqual 2 anotherValue


data TestState = TestState { tsCache :: Cache String Int }

instance HasCache String Int TestState where
    getCache = tsCache


test_withCacheM = do
    -- Test using cache via a monad
    state <- liftM TestState $ initCache

    -- Test caching a function that depends on internal state
    values <- newMVar [1, 2, 3]
    let nextValue :: IO Int
        nextValue = modifyMVar values $ \(v:vs) -> return (vs, v)

    let nextValue' = liftIO nextValue

    flip evalStateT state $ do
        -- The first call should call the function and get 1
        firstValue <- withCacheM "mykey" nextValue'
        liftIO $ assertEqual 1 firstValue

        -- Same cache key, this should return 1
        cachedValue <- withCacheM "mykey" nextValue'
        liftIO $ assertEqual 1 cachedValue

        -- Different cache key
        anotherValue <- withCacheM "anotherkey" nextValue'
        liftIO $ assertEqual 2 anotherValue