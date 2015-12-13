{-# LANGUAGE MultiParamTypeClasses #-}
module Cache (
    Cache,
    initCache,
    withCache,
) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as M

-- Opaque cache type storing values of type v against keys of type k
type Cache k v = MVar (Map k v)

-- Initialize an empty cache
initCache :: MonadIO m => m (Cache k v)
initCache = liftIO $ newMVar M.empty

-- Cache the result of monadic action under a certain key
withCache :: (MonadIO m, Ord k) => Cache k v -> k -> m v -> m v
withCache cache key action = do
    values <- liftIO $ readMVar cache
    case M.lookup key values of
      Just val -> return val
      Nothing -> do
          val <- action
          liftIO $ modifyMVar_ cache $ return . insertIfMissing key val
          return val

-- Insert the new value only if there's not an existing one
insertIfMissing :: Ord k => k -> v -> Map k v -> Map k v
insertIfMissing = M.insertWith (flip const)
