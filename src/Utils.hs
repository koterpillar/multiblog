module Utils where

import Control.Monad

import qualified Data.Map as M


-- Read a value or monadically fail
readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(v, _)] -> return v
    _ -> fail "No parse"

-- Convert Maybe to Either, with a supplied error message
require :: String -> Maybe a -> Either String a
require _ (Just v) = Right v
require err Nothing = Left err

-- Are all Eithers in the Map Right?
mapAllRight :: M.Map k (Either e v) -> Either e (M.Map k v)
mapAllRight m = let (bad, good) = M.mapEither id m in case M.toList bad of
    [] -> Right good
    (_, err):_ -> Left err

-- Extract the single value from a list, or return mzero
onlyOne :: MonadPlus m => m [a] -> m a
onlyOne action = do
    ms <- action
    case ms of
        [m] -> return m
        _ -> mzero

-- Whether a file path is special, to avoid infinite recursion
isSpecial :: FilePath -> Bool
isSpecial "." = True
isSpecial ".." = True
isSpecial _ = False

reverseCompare :: Ord a => a -> a -> Ordering
reverseCompare = flip compare

-- Group items by a function
groupBy :: (Eq k, Ord k) => (a -> k) -> [a] -> [[a]]
groupBy key = M.elems . foldr (uncurry $ M.insertWith (++)) M.empty . map addKey
    where addKey v = (key v, [v])
