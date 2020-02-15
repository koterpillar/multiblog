{-|
Types related to the external services.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types.Services where

import Control.Monad.Reader

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Default.Class
import qualified Data.Map as M
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Data.Yaml

import GHC.Generics (Generic)

import qualified Web.Twitter.Conduit as TW

import Types.Language

newtype AppServices = AppServices
    { asTwitter :: Maybe TW.OAuth
    } deriving (Generic, Show)

class HasAppServices a where
    getAppServices :: a -> AppServices

instance Default AppServices

instance FromJSON TW.OAuth where
    parseJSON =
        A.withObject "Object expected" $ \v -> do
            key <- v .: "consumer_key"
            secret <- v .: "consumer_secret"
            return $
                TW.twitterOAuth
                { TW.oauthConsumerKey = Text.encodeUtf8 key
                , TW.oauthConsumerSecret = Text.encodeUtf8 secret
                }

instance FromJSON AppServices where
    parseJSON =
        A.withObject "Object expected" $ \v -> AppServices <$> v .:? "twitter"

newtype AppAuth =
    AppAuthTwitter TwitterAuth
    deriving (Eq, Show, Generic)

class ToJSON a => ServiceAuth a where
    toAppAuth :: a -> AppAuth
    fromAppAuth :: AppAuth -> Maybe a
    parseAuth :: Object -> Parser a

data TwitterAuth = TwitterAuth
    { taToken :: BS.ByteString
    , taSecret :: BS.ByteString
    } deriving (Eq, Show, Generic)

instance ServiceAuth TwitterAuth where
    toAppAuth = AppAuthTwitter
    fromAppAuth (AppAuthTwitter a) = Just a
    parseAuth v =
        let encodeUtf = fmap Text.encodeUtf8
        in TwitterAuth <$> encodeUtf (v .: "oauth_token") <*>
           encodeUtf (v .: "oauth_token_secret")

instance ToJSON TwitterAuth where
    toJSON (TwitterAuth token secret) =
        object
            [ "oauth_token" .= Text.decodeUtf8 token
            , "oauth_token_secret" .= Text.decodeUtf8 secret
            ]

taCredential :: TwitterAuth -> TW.Credential
taCredential ta =
    TW.Credential
        [("oauth_token", taToken ta), ("oauth_token_secret", taSecret ta)]

twitterAuth :: TW.Credential -> TwitterAuth
twitterAuth (TW.Credential cred) =
    let credMap = M.fromList cred
    in TwitterAuth
       { taToken = fromJust $ M.lookup "oauth_token" credMap
       , taSecret = fromJust $ M.lookup "oauth_token_secret" credMap
       }

parseAppAuth :: Object -> Parser AppAuth
parseAppAuth v = AppAuthTwitter <$> parseAuth v

instance ToJSON AppAuth where
    toJSON (AppAuthTwitter a) = toJSON a

data CrossPost = CrossPost
    { cpLanguage :: Language
    , cpServiceDetails :: AppAuth
    } deriving (Eq, Show, Generic)

type AppCrossPost = [CrossPost]

class HasCrossPosts a where
    getCrossPosts :: a -> AppCrossPost

instance FromJSON CrossPost where
    parseJSON =
        A.withObject "Object expected" $ \v -> do
            lang <- v .: "lang"
            auth <- parseAppAuth v
            return $ CrossPost lang auth

withTwitter :: (HasAppServices a, MonadReader a m) => (TW.OAuth -> m r) -> m r
withTwitter act = do
    twitter <- asks (asTwitter . getAppServices)
    case twitter of
        Nothing -> error "Twitter credentials not defined"
        Just auth -> act auth

withCreds ::
       (ServiceAuth sa, HasCrossPosts a, MonadReader a m)
    => (Language -> sa -> m b)
    -> m [b]
withCreds act = do
    cps <- asks (mapMaybe filterService . getCrossPosts)
    traverse (uncurry act) cps
  where
    filterService crossPost =
        case fromAppAuth (cpServiceDetails crossPost) of
            Nothing -> Nothing
            Just cred -> Just (cpLanguage crossPost, cred)
