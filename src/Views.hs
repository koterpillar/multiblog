module Views where

import Happstack.Server

import App
import Models

articleListDisplay :: [Article] -> ServerPartT App Response
articleListDisplay = ok . toResponse . show . map arContent

articleDisplay :: Article -> ServerPartT App Response
articleDisplay = ok . toResponse . arContent
