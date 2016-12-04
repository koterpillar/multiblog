{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Services where

import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as U
import Data.Default.Class
import qualified Data.Map as M
import Data.Maybe
import Data.Yaml

import GHC.Generics (Generic)

import qualified Web.Twitter.Conduit as TW

import Types.Language

data AppServices = AppServices
    { asTwitter :: Maybe TW.OAuth
    } deriving (Generic, Show)

instance Default AppServices

instance FromJSON TW.OAuth where
    parseJSON =
        A.withObject "Object expected" $
        \v -> do
            key <- v .: "consumer_key"
            secret <- v .: "consumer_secret"
            return $
                TW.twitterOAuth
                { TW.oauthConsumerKey = U.fromString key
                , TW.oauthConsumerSecret = U.fromString secret
                }

instance FromJSON AppServices where
    parseJSON =
        A.withObject "Object expected" $ \v -> AppServices <$> v .:? "twitter"

data AppAuth =
    AppAuthTwitter TwitterAuth
    deriving (Eq, Show)

class ToJSON a =>
      ServiceAuth a  where
    toAppAuth :: a -> AppAuth
    fromAppAuth :: AppAuth -> Maybe a
    parseAuth :: Object -> Parser a

data TwitterAuth = TwitterAuth
    { taToken :: U.ByteString
    , taSecret :: U.ByteString
    } deriving (Eq, Show)

instance ServiceAuth TwitterAuth where
    toAppAuth = AppAuthTwitter
    fromAppAuth (AppAuthTwitter a) = Just a
    parseAuth v =
        let decodeUtf = fmap U.fromString
        in TwitterAuth <$> decodeUtf (v .: "oauth_token") <*>
           decodeUtf (v .: "oauth_token_secret")

instance ToJSON TwitterAuth where
    toJSON (TwitterAuth token secret) =
        object
            [ "oauth_token" .= U.toString token
            , "oauth_token_secret" .= U.toString secret
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
    } deriving (Eq, Show)

type AppCrossPost = [CrossPost]

instance FromJSON CrossPost where
    parseJSON =
        A.withObject "Object expected" $
        \v -> do
            lang <- v .: "lang"
            auth <- parseAppAuth v
            return $ CrossPost lang auth

withTwitter :: AppServices -> (TW.OAuth -> a) -> a
withTwitter services act =
    case asTwitter services of
        Nothing -> error "Twitter credentials not defined"
        Just auth -> act auth

withCreds
    :: (ServiceAuth a, Applicative m)
    => AppCrossPost -> (Language -> a -> m b) -> m [b]
withCreds cps act = traverse (uncurry act) $ mapMaybe filterService cps
  where
    filterService crossPost =
        case fromAppAuth (cpServiceDetails crossPost) of
            Nothing -> Nothing
            Just cred -> Just (cpLanguage crossPost, cred)
