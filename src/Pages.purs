module Pages where

import Doc
import Prelude

import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import DocBuilder (fancyExternalLink)
import Effect.Aff (Aff)

pages :: Map PageId (Aff Page)
pages = Map.fromFoldable $ map (lmap makePageId)
  [ "hello-world" /\ do
      pure $
        Page
          { abstract:
              String "this is the 'hello-world' page"
          , kids: [ String "hello world!" ]
          }

  , "external-link-ex1" /\ do
      kids <- sequence
        [ fancyExternalLink
            { href: "https://hazyresearch.stanford.edu/blog/2022-01-14-s4-1"
            , mb_label: pure $ String "Structured State Spaces for Sequence Modeling (S4)"
            }
        ]
      pure $
        Page
          { abstract: String "this demonstrates a fancy link that finds an excerpt and a favicon during compilation."
          , kids
          }
  ]
