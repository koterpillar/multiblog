{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Cache (
    Cache,
    CacheMonad,
    HasCache,
    getCache,
    getCacheM,
    initCache,
    withCache,
    withCacheM,
) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M

-- Opaque cache type storing values of type v against keys of type k
data Cache k v = Cache (MVar (Map k v))

instance Show (Cache k v) where
    show _ = "(cache)"

-- Initialize an empty cache
initCache :: MonadIO m => m (Cache k v)
initCache = liftM Cache $ liftIO $ newMVar M.empty

-- Cache the result of monadic action under a certain key
withCache :: (MonadIO m, Ord k) => Cache k v -> k -> m v -> m v
withCache (Cache cache) key action = do
    values <- liftIO $ readMVar cache
    case M.lookup key values of
      Just val -> return val
      Nothing -> do
          val <- action
          liftIO $ modifyMVar_ cache $ return . insertIfMissing key val
          return val

-- Class for data structures with a cache field
class HasCache k v a where
    getCache :: a -> Cache k v

-- Monads that have caches
class CacheMonad k v m where
    getCacheM :: m (Cache k v)

-- If the state has cache, it can be used
instance (MonadState s m, HasCache k v s) => CacheMonad k v m where
    getCacheM = gets getCache

withCacheM :: (MonadIO m, CacheMonad k v m, Ord k) => k -> m v -> m v
withCacheM key action = do
    cache <- getCacheM
    withCache cache key action

-- Insert the new value only if there's not an existing one
insertIfMissing :: Ord k => k -> v -> Map k v -> Map k v
insertIfMissing = M.insertWith (flip const)
