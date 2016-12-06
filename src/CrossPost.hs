{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module CrossPost where

import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Data.Text as T

import Web.Routes
import Web.Twitter.Conduit

import App
import Models
import Routes
import Views
import Types.Content
import Types.Language
import Types.Services

crossPost :: App ()
crossPost = do
    runRoute $
        do arts <- asks appArticles
           case arts of
               [] -> error "No articles to post"
               _ -> crossPostTwitter (maximum arts)

crossPostTwitter
    :: (MonadRoute m, URL m ~ Sitemap, MonadIO m, MonadReader AppData m)
    => Article -> m ()
crossPostTwitter art = do
    mgr <- liftIO $ newManager tlsManagerSettings
    _ <-
        withCreds $
        \credLang cred -> do
            withTwitter $
                \auth -> do
                    let twInfo = mkTwitterAuth auth cred
                    let lpref = singleLanguage credLang
                    let title = langTitle lpref art
                    articleLink <- linkTo art
                    address <- asks appAddress
                    let content = title ++ " " ++ address ++ articleLink
                    liftIO $ call twInfo mgr $ update (T.pack content)
    return ()

mkTwitterAuth :: OAuth -> TwitterAuth -> TWInfo
mkTwitterAuth auth cred =
    def
    { twToken =
        def
        { twOAuth = auth
        , twCredential = taCredential cred
        }
    }
