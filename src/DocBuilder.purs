module DocBuilder where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable as Nullable
import Doc (Doc(..))
import Effect.Aff (Aff)
import Tools.Readability (getFaviconUrlFromUrl, loadReadabilityArticleFromUrl)

fancyExternalLink :: { href :: String, mb_label :: Maybe Doc } -> Aff Doc
fancyExternalLink { href, mb_label } = do
  mb_icon_src <- getFaviconUrlFromUrl href
  mb_article <- loadReadabilityArticleFromUrl href
  let mb_abstract = mb_article >>= _.excerpt >>> Nullable.toMaybe
  pure $ ExternalLink
    { href
    , mb_label
    , mb_abstract
    , mb_icon_src
    }

