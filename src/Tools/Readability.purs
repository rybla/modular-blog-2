module Tools.Readability where

import Prelude

import Control.Applicative (pure)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch (Method(..), fetch)
import Promise (Promise)
import Promise.Aff (toAffE)
import Tools.ReadabilityPure

loadReadabilityArticleFromUrl :: String -> Aff (Maybe ReadabilityArticle)
loadReadabilityArticleFromUrl url = loadReadabilityFromUrl url >>= ((_ >>= parse) >>> pure)

loadReadabilityFromUrl :: String -> Aff (Maybe Readability)
loadReadabilityFromUrl url = loadReadabilityFromUrl_ _maybe url # toAffE

foreign import loadReadabilityFromUrl_
  :: { "Nothing" :: forall a. Maybe a, "Just" :: forall a. a -> Maybe a }
  -> String
  -> Effect (Promise (Maybe Readability))

parse :: Readability -> Maybe ReadabilityArticle
parse = parse_ _maybe

foreign import parse_
  :: { "Nothing" :: forall a. Maybe a, "Just" :: forall a. a -> Maybe a }
  -> Readability
  -> Maybe ReadabilityArticle

_maybe
  :: { "Nothing" :: forall a. Maybe a
     , "Just" :: forall a. a -> Maybe a
     }
_maybe = { "Just": Just, "Nothing": Nothing }

getFaviconUrlFromUrl :: String -> Aff (Maybe String)
getFaviconUrlFromUrl url = do
  let favicon_url = "http://www.google.com/s2/favicons?domain=" <> url
  response <- fetch favicon_url { method: GET }
  if not response.ok then pure empty
  else
    pure (pure favicon_url)
