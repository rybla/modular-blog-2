module Main where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Doc (Doc(..), GroupStyle(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)

main :: Effect Unit
main = do
  let str = toJsonString (Group { style: Row, kids: [ String "hello", String "world" ] })
  log $ "str : " <> str
  let filename = "./docs/tmp.json"
  writeTextFile UTF8 filename str
  str' <- readTextFile UTF8 filename
  log $ "str': " <> str'

