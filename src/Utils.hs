module Utils where

import qualified Data.Map as M


-- Read a value or monadically fail
readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(v, _)] -> return v
    _ -> fail "No parse"

-- Convert Maybe to Either, with a supplied error message
mfe :: String -> Maybe a -> Either String a
mfe _ (Just v) = Right v
mfe err Nothing = Left err

-- Are all Eithers in the Map Right?
mapAllRight :: M.Map k (Either e v) -> Either e (M.Map k v)
mapAllRight m = let (bad, good) = M.mapEither id m in case M.toList bad of
    [] -> Right good
    (_, err):_ -> Left err
