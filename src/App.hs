module App where

import Control.Monad.State

import Import
import Models


type App = StateT AppState IO

runApp :: App a -> IO a
runApp a = do
    loaded <- loadFromDirectory "content"
    case loaded of
        Left err -> error err
        Right appState -> do
            print appState
            evalStateT a appState
