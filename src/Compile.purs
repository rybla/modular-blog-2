module Compile where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Doc (fromDocIdToJsonFilePath, fromPageIdToJsonFilePath)
import Docs (docs)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Pages (pages)

main :: Effect Unit
main = launchAff_ do
  let pageIds = pages # Map.keys # Array.fromFoldable
  writeTextFile UTF8 "./docs/pageIds.json" (toJsonString pageIds)
  compilePages
  compileDocs
  pure unit

compilePages :: Aff Unit
compilePages = do
  pages # traverseWithIndex_ \pageId aff_page -> do
    Console.log $ "compiling page " <> show pageId <> " ..."
    page <- aff_page
    let filepath = fromPageIdToJsonFilePath pageId
    writeTextFile UTF8
      filepath
      (toJsonString page)
    Console.log $ "compiled page " <> show pageId <> " to " <> show filepath
    pure unit

compileDocs :: Aff Unit
compileDocs = do
  docs # traverseWithIndex_ \docId aff_doc -> do
    Console.log $ "compiling doc " <> show docId <> " ..."
    doc <- aff_doc
    let filepath = fromDocIdToJsonFilePath docId
    writeTextFile UTF8
      filepath
      (toJsonString doc)
    Console.log $ "compiled doc " <> show docId <> " to " <> show filepath
    pure unit

