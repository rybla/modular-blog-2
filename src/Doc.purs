module Doc where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Node.Path (FilePath)
import Utility (bug)

--------------------------------------------------------------------------------
-- Doc
--------------------------------------------------------------------------------

data Doc
  = String String
  | Group Group

derive instance Generic Doc _

instance EncodeJson Doc where
  encodeJson x = genericEncodeJson x

instance DecodeJson Doc where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------
-- Group
--------------------------------------------------------------------------------

type Group = { style :: GroupStyle, kids :: Array Doc }

data GroupStyle
  = Column
  | Row

derive instance Generic GroupStyle _

instance EncodeJson GroupStyle where
  encodeJson x = genericEncodeJson x

instance DecodeJson GroupStyle where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------
-- DocQuery 
--------------------------------------------------------------------------------

data DocQuery = Ref { id :: DocId }

derive instance Generic DocQuery _

instance EncodeJson DocQuery where
  encodeJson x = genericEncodeJson x

instance DecodeJson DocQuery where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------
-- DocId 
--------------------------------------------------------------------------------

newtype DocId = DocId String

derive instance Generic DocId _

derive newtype instance Show DocId

derive newtype instance Eq DocId

derive newtype instance Ord DocId

instance EncodeJson DocId where
  encodeJson x = genericEncodeJson x

instance DecodeJson DocId where
  decodeJson x = genericDecodeJson x

makeDocId :: String -> DocId
makeDocId str | isValidDocId str = DocId str
makeDocId str = bug $ "invalid DocId: " <> str

isValidDocId :: String -> Boolean
isValidDocId _str = true -- TODO: make sure is valid filename

fromDocIdToJsonFilePath :: DocId -> FilePath
fromDocIdToJsonFilePath (DocId str) = "./docs/docs/" <> str <> ".json"

--------------------------------------------------------------------------------
-- PageId
--------------------------------------------------------------------------------

newtype PageId = PageId String

derive instance Generic PageId _

derive newtype instance Show PageId

derive newtype instance Eq PageId

derive newtype instance Ord PageId

instance EncodeJson PageId where
  encodeJson x = genericEncodeJson x

instance DecodeJson PageId where
  decodeJson x = genericDecodeJson x

makePageId :: String -> PageId
makePageId str | isValidPageId str = PageId str
makePageId str = bug $ "invalid PageId: " <> str

isValidPageId :: String -> Boolean
isValidPageId _str = true -- TODO: make sure is valid filename

fromPageIdToJsonFilePath :: PageId -> FilePath
fromPageIdToJsonFilePath (PageId str) = "./docs/pages/" <> str <> ".json"

