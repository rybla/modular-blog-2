module Tools.Readability where

import Prelude

import Control.Applicative (pure)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import data Readability :: Type

type ReadabilityArticle =
  { title :: Nullable String
  , content :: Nullable String
  , textContent :: Nullable String
  , length :: Nullable Number
  , excerpt :: Nullable String
  , byline :: Nullable String
  , dir :: Nullable String
  , siteName :: Nullable String
  , lang :: Nullable String
  , publishedTime :: Nullable String
  }

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

