module Docs where

import Doc
import Prelude

import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)

docs :: Map DocId (Aff Doc)
docs = Map.fromFoldable $ map (lmap makeDocId)
  [ "hello-world" /\
      pure (String "hello world!")
  ]
