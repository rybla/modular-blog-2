module Main where

import Prelude

import Control.Plus (empty)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Tools.Readability (loadReadabilityFromUrl, parse)

main :: Effect Unit
main = do
  -- let str = toJsonString (Group { style: Row, kids: [ String "hello", String "world" ] })
  -- log $ "str : " <> str
  -- let filename = "./docs/tmp.json"
  -- writeTextFile UTF8 filename str
  -- str' <- readTextFile UTF8 filename
  -- log $ "str': " <> str'

  launchAff_ do
    -- let url = "https://hazyresearch.stanford.edu/blog/2022-01-14-s4-1"
    let url = "https://google.com"
    readability <- loadReadabilityFromUrl url >>= maybe empty pure
    article <- readability # parse # maybe empty pure
    -- Console.logShow { article }
    Console.logShow { "article.title": article.title }
    Console.logShow { "article.siteName": article.siteName }
    Console.logShow { "article.excerpt": article.excerpt }
    pure unit

  pure unit

