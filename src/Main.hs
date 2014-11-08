module Main where

import Control.Monad

import Data.Time

import Happstack.Server


main :: IO ()
main = simpleHTTP nullConf $ msum [ mzero
                                  , index
                                  , article
                                  ]

data Article = Article { arTitle :: String
                       , arContent :: Content
                       , arAuthored :: UniversalTime
                       }

type Content = String

index :: ServerPartT IO Response
index = articleList $ const True

-- TODO: Monthly, daily indices

articleList :: (Article -> Bool) -> ServerPartT IO Response
articleList = undefined

dayPath :: (ServerMonad m, MonadPlus m) => (Day -> m b) -> m b
dayPath handler =
    path $ \year ->
    path $ \month ->
    path $ \day -> case fromGregorianValid year month day of
        Nothing -> mzero
        Just date -> handler date

article :: ServerPartT IO Response
article = dayPath $ \date -> path $ \slug -> do
    nullDir
    ok $ toResponse (slug :: String)

articleDisplay :: Article -> ServerPartT IO Response
articleDisplay = undefined
