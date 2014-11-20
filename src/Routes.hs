{-# Language TemplateHaskell #-}
module Routes where

import Data.Time

import Web.Routes
import Web.Routes.TH (derivePathInfo)

instance PathInfo Day where
    toPathSegments = undefined
    fromPathSegments = undefined

data Sitemap = VHome
             | VYearly Integer
             | VMonthly Integer Int
             | VDaily Day
             | VArticle Day String

derivePathInfo ''Sitemap
