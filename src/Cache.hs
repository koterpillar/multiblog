{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Cache
    ( Cache
    , HasCache
    , getCache
    , initCache
    , withCache
    , withCacheM
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Data.Map                (Map)
import qualified Data.Map                as Map

-- Opaque cache type storing values of type v against keys of type k
newtype Cache k v =
    Cache (MVar (Map k v))

instance Show (Cache k v) where
    show _ = "(cache)"

-- Initialize an empty cache
initCache :: MonadIO m => m (Cache k v)
initCache = fmap Cache $ liftIO $ newMVar Map.empty

-- Cache the result of monadic action under a certain key
withCache :: (MonadIO m, Ord k) => Cache k v -> k -> m v -> m v
withCache (Cache cache) key action = do
    values <- liftIO $ readMVar cache
    case Map.lookup key values of
        Just val -> return val
        Nothing -> do
            val <- action
            liftIO $ modifyMVar_ cache $ return . insertIfMissing key val
            return val

-- Class for data structures with a cache field
class HasCache k v a | a -> k v where
    getCache :: a -> Cache k v

withCacheM ::
       (MonadIO m, MonadState s m, HasCache k v s, Ord k) => k -> m v -> m v
withCacheM key action = do
    cache <- gets getCache
    withCache cache key action

-- Insert the new value only if there's not an existing one
insertIfMissing :: Ord k => k -> v -> Map k v -> Map k v
insertIfMissing = Map.insertWith (flip const)
