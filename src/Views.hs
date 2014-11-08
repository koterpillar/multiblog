module Views where

import Happstack.Server

import App

articleListDisplay :: [Article] -> ServerPartT App Response
articleListDisplay = ok . toResponse . show . map arContent

articleDisplay :: Article -> ServerPartT App Response
articleDisplay = ok . toResponse . arContent
