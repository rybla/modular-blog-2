module Tools.ReadabilityPure where

import Prelude

import Data.Nullable (Nullable)

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
