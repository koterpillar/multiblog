{-|
Action to cross-post the latest article to all external services.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module CrossPost where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import qualified Data.Aeson                     as A
import           Data.Default.Class
import           Data.List
import           Data.Maybe
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T

import           Network.HTTP.Conduit           (newManager, tlsManagerSettings)

import           Web.Twitter.Conduit            (OAuth, TWInfo (..), call,
                                                 twCredential, twOAuth)
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Conduit.Parameters hiding (map)
import           Web.Twitter.Conduit.Request    (APIRequest)
import           Web.Twitter.Conduit.Status
import           Web.Twitter.Types

import           App
import           Models
import           Routes
import           Types.Content
import           Types.Language
import           Types.Services
import           Views

crossPost :: App ()
crossPost = do
    liftIO $ putStrLn "Cross-posting new articles..."
    crossPostTwitter
    liftIO $ putStrLn "All new articles cross-posted."

crossPostTwitter :: (MonadIO m, MonadReader AppData m) => m ()
crossPostTwitter = do
    mgr <- liftIO $ newManager tlsManagerSettings
    address <- asks appAddress
    _ <-
        withTwitterAuth $ \credLang twInfo -> do
            let doCall :: (A.FromJSON b, MonadIO m1) => APIRequest a b -> m1 b
                doCall = liftIO . call twInfo mgr
            -- Get account ID
            accountId <-
                fmap userId $
                doCall $
                accountVerifyCredentials & (includeEntities ?~ False) &
                (skipStatus ?~ False)
            -- Get existing posts and collect all articles already linked
            existingPosts <- doCall $ userTimeline (UserIdParam accountId)
            existingArticles <- twitterArticleLinks existingPosts
            -- Filter only articles newer than anything posted
            let lastExistingArticle = maximum existingArticles
            unposted <- sort <$> askFiltered (lastExistingArticle <)
            -- Post the remaining articles
            let lpref = singleLanguage credLang
            forM unposted $ \art -> do
                let title = langTitle lpref art
                articleLink <- linkTo art
                let content = title <> " " <> address <> articleLink
                doCall $ update content
    return ()

twitterArticleLinks :: (MonadReader AppData m) => [Status] -> m [Article]
twitterArticleLinks statuses = do
    addr <- asks appAddress
    let relativeUrl aurl
            | T.isPrefixOf addr aurl = Just $ T.drop (T.length addr) aurl
            | otherwise = Nothing
    -- Extract all the links pointing to our site
    let urls = mapMaybe relativeUrl $ concatMap statusLinks statuses
    -- Filter out the ones for the article route
    let articleLinks =
            [(date, slug) | Just (ArticleView date slug) <- map parseURL urls]
    -- Get the matched articles
    let articleFilter art = any (\(d, s) -> byDateSlug d s art) articleLinks
    askFiltered articleFilter

statusLinks :: Status -> [T.Text]
statusLinks status = do
    entity <- maybeToList $ statusEntities status
    eurl <- enURLs entity
    let urlEntity = ueExpanded $ entityBody eurl
    return urlEntity

mkTwitterAuth :: OAuth -> TwitterAuth -> TWInfo
mkTwitterAuth auth cred =
    def {twToken = def {twOAuth = auth, twCredential = taCredential cred}}

withTwitterAuth :: MonadReader AppData m => (Language -> TWInfo -> m b) -> m [b]
withTwitterAuth act =
    withCreds $ \credLang cred ->
        withTwitter $ \auth -> do
            let twInfo = mkTwitterAuth auth cred
            act credLang twInfo
