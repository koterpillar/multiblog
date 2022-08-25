{-# LANGUAGE TemplateHaskell #-}

module Views where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Default.Class
import           Data.LanguageCodes
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.String.Here         (i)
import           Data.Text                (Text)
import qualified Data.Text.Lazy           as TL
import           Data.Time
import           Data.Time.Format.ISO8601

import           Text.Blaze.Html          (Markup, preEscapedToHtml)
import           Text.Hamlet
import           Text.Julius
import           Text.Lucius
import           Text.Pandoc              hiding (Meta, Reader)
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Walk

import           Models
import           Render
import           Routes
import           Types.Content
import           Types.Language

class Linkable a where
    link :: a -> Sitemap

instance Linkable Sitemap where
    link = id

instance Linkable Article where
    link a = ArticleView (utctDay $ arAuthored a) (arSlug a)

instance Linkable Meta where
    link m = MetaView (mtSlug m) Nothing

data OGType
    = OGWebsite
    | OGArticle
          { ogaPublished :: UTCTime
          }

instance Default OGType where
    def = OGWebsite

data PageContent =
    PageContent
        { pcURL     :: Text
        , pcTitle   :: Maybe Text
        , pcType    :: OGType -- ^ https://ogp.me/#types
        , pcLayout  :: Layout
        , pcContent :: HtmlUrl Sitemap
        }

mkPageContent :: Text -> PageContent
mkPageContent url =
    PageContent
        { pcURL = url
        , pcTitle = def
        , pcType = def
        , pcLayout = def
        , pcContent = mempty
        }

type PageNumber = Int

data Paginated a =
    Paginated
        { pagePrev  :: Maybe PageNumber
        , pageItems :: [a]
        , pageNext  :: Maybe PageNumber
        }
    deriving (Eq, Show)

pageSize :: Int
pageSize = 10

paginate :: Int -> PageNumber -> [a] -> Paginated a
paginate size page allItems = Paginated prev items next
  where
    prev
        | page == 1 = Nothing
        | otherwise = Just (page - 1)
    offsetItems = drop ((page - 1) * size) allItems
    (items, rest) = splitAt size offsetItems
    next
        | null rest = Nothing
        | otherwise = Just (page + 1)

render :: MonadReader AppData m => HtmlUrl Sitemap -> m Markup
render html = html <$> routeURLParams

askLangStringFn ::
       MonadReader AppData m => LanguagePreference -> m (Text -> Text)
askLangStringFn lang = do
    strings <- asks appStrings
    let fn str = fromMaybe str $ Map.lookup str strings >>= matchLanguage lang
    pure fn

askLangString :: MonadReader AppData m => LanguagePreference -> Text -> m Text
askLangString lang str = askLangStringFn lang >>= (\fn -> pure $ fn str)

linkTitle ::
       (MonadReader AppData m, MonadPlus m)
    => LanguagePreference
    -> Link
    -> m Text
linkTitle lang (ExternalLink url titles) =
    pure $ fromMaybe url $ matchLanguage lang titles
linkTitle lang (MetaLink slug) = langTitle lang <$> askMeta slug

linkDestination :: (MonadReader AppData m, MonadPlus m) => Link -> m Text
linkDestination (ExternalLink url _) = pure url
linkDestination (MetaLink slug)      = askMeta slug >>= linkTo

template ::
       (MonadReader AppData m, MonadPlus m)
    => LanguagePreference
    -> PageContent
    -> m Markup
template lang page = do
    links <- asks appLinks
    linkTitleUrls <-
        forM links $ \l -> do
            title <- linkTitle lang l
            destination <- linkDestination l
            return (title, destination)
    allLangs <- asks allLanguages
    langString <- askLangStringFn lang
    AppSettings {..} <- asks appSettings
    case pcLayout page of
        BaseLayout -> render $(hamletFile "templates/base.hamlet")
        PresentationLayout ->
            render $(hamletFile "templates/base_presentation.hamlet")

articleListDisplay ::
       (MonadReader AppData m, MonadPlus m)
    => LanguagePreference
    -> Paginated Article
    -> m Markup
articleListDisplay lang articles = do
    url <- linkTo Index
    articlesContent <- mapM (linkedContent lang) (pageItems articles)
    langString <- askLangStringFn lang
    template lang $
        (mkPageContent url) {pcContent = $(hamletFile "templates/list.hamlet")}

articleDisplay ::
       (MonadReader AppData m, MonadPlus m)
    => LanguagePreference
    -> Article
    -> m Markup
articleDisplay lang article = do
    url <- linkTo article
    template lang $
        (mkPageContent url)
            { pcTitle = Just $ langTitle lang article
            , pcType = OGArticle {ogaPublished = arAuthored article}
            , pcContent = $(hamletFile "templates/article.hamlet")
            }

metaDisplay ::
       (MonadReader AppData m, MonadPlus m)
    => LanguagePreference
    -> Meta
    -> m Markup
metaDisplay lang meta = do
    url <- linkTo meta
    let content =
            case mtLayout meta of
                BaseLayout -> $(hamletFile "templates/meta.hamlet")
                PresentationLayout ->
                    $(hamletFile "templates/meta_presentation.hamlet")
    template lang $
        (mkPageContent url)
            { pcTitle = Just $ langTitle lang meta
            , pcLayout = mtLayout meta
            , pcContent = content
            }

-- Generate a link to some content
linkTo :: (MonadReader AppData m, Linkable a) => a -> m Text
linkTo a = do
    r <- routeURLParams
    pure $ r (link a) []

-- Modify the content to have a link to itself and have no anchors
linkedContent ::
       (MonadReader AppData m, HasContent a, Linkable a)
    => LanguagePreference
    -> a
    -> m Pandoc
linkedContent lang a = do
    lnk <- linkTo a
    pure $ linkedHeader lnk $ langContent lang a

-- Modify the first header to be a link to a given place
-- and remove all anchors from headers
linkedHeader :: Text -> Pandoc -> Pandoc
linkedHeader target doc = evalState (walkM linkHeader doc) True
  where
    linkHeader :: Block -> State Bool Block
    linkHeader (Header n _ text)
               -- note if this is the first header
     = do
        isFirst <- get
        put False
        -- make the first header a link
        let text' =
                if isFirst
                    then [Link nullAttr text (target, "")]
                    else text
        -- remove anchors
        return $ Header n ("", [], []) text'
    linkHeader x = return x

routeURLParams :: MonadReader AppData m => m (Render Sitemap)
routeURLParams = do
    address <- asks appAddress
    pure $ \r _ -> address <> routeURL r

renderSiteScript :: MonadReader AppData m => m JavaScript
renderSiteScript =
    routeURLParams >>= \r ->
        pure $
        JavaScript $ renderJavascriptUrl r $(juliusFile "templates/site.julius")

renderPrintStylesheet :: MonadReader AppData m => m Stylesheet
renderPrintStylesheet =
    routeURLParams >>= \r ->
        pure $
        Stylesheet $ renderCssUrl r $(luciusFile "templates/print.lucius")

renderCodeStylesheet :: Stylesheet
renderCodeStylesheet = Stylesheet $ TL.pack $ styleToCss highlightingStyle

-- | Presentation can only render the pipe tables. Disable the other kinds
presentationOptions :: WriterOptions
presentationOptions = def {writerSlideLevel = Just 1}

cdn :: Text -> Text -> Text -> Text
cdn package version asset = [i|https://cdn.jsdelivr.net/npm/${package}@${version}/${asset}|]

revealCdn :: Text -> Text
revealCdn = cdn "reveal.js" "4" . ("dist/" <>)

bootstrapCdn :: Text -> Text
bootstrapCdn = cdn "bootstrap" "3" . ("dist/" <>)
