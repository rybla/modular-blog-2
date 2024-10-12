module Pages where

import Doc
import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Page (Page(..))

pages :: Map PageId (Aff Page)
pages = Map.fromFoldable $ map (lmap makePageId)
  [ "hello-world" /\
      pure (Page { abstract: String "this is the 'hello-world' page", kids: [ String "hello world!" ] })
  ]
