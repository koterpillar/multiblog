module TestCache where

import           Control.Concurrent.MVar
import           Control.Monad.State     hiding (state)

import           Data.Text               (Text)

import           Cache

import           Test.HUnit

unit_cached :: IO ()
unit_cached = do
    values <- newMVar [1, 2, 3]
    let nextValue :: IO Int
        nextValue = modifyMVar values $ \(v:vs) -> return (vs, v)
    cache <- initCache :: IO (Cache String Int)
    -- The first call should call the function and get 1
    firstValue <- withCache cache "mykey" nextValue
    assertEqual "" 1 firstValue
    -- Same cache key, this should return 1
    cachedValue <- withCache cache "mykey" nextValue
    assertEqual "" 1 cachedValue
    -- Different cache key
    anotherValue <- withCache cache "anotherkey" nextValue
    assertEqual "" 2 anotherValue

-- Test caching a function that depends on internal state
newtype TestState =
    TestState
        { tsCache :: Cache Text Int
        }

instance HasCache Text Int TestState where
    getCache = tsCache

unit_withCacheM :: IO ()
unit_withCacheM
-- Test using cache via a monad
 = do
    state <- TestState <$> initCache
    -- Test caching a function that depends on internal state
    values <- newMVar [1, 2, 3]
    let nextValueIO :: IO Int
        nextValueIO = modifyMVar values $ \(v:vs) -> return (vs, v)
    let nextValue = liftIO nextValueIO
    flip evalStateT state $ do
        firstValue <- withCacheM "mykey" nextValue
        liftIO $ assertEqual "should call the function" 1 firstValue
        cachedValue <- withCacheM "mykey" nextValue
        liftIO $ assertEqual "same cache key" 1 cachedValue
        anotherValue <- withCacheM "anotherkey" nextValue
        liftIO $ assertEqual "different cache key" 2 anotherValue
