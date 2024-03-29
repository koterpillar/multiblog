module Utils where

import           Control.Monad

import qualified Data.Map      as Map

-- Read a value or monadically fail
readM :: (MonadFail m, Read a) => String -> m a
readM s =
    case reads s of
        [(v, _)] -> return v
        _        -> fail "No parse"

-- Extract the single value from a list, or return mzero
onlyOne :: MonadPlus m => m [a] -> m a
onlyOne action = do
    ms <- action
    case ms of
        [m] -> return m
        _   -> mzero

reverseCompare :: Ord a => a -> a -> Ordering
reverseCompare = flip compare

-- Group items by a function
groupBy :: Ord k => (a -> k) -> [a] -> [[a]]
groupBy key =
    Map.elems . foldr (uncurry (Map.insertWith (++)) . addKey) Map.empty
  where
    addKey v = (key v, [v])
