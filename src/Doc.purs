module Doc where

import Prelude

import Control.Alternative (empty, guard)
import Control.Plus (empty)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect.Aff (Aff)
import Node.Path (FilePath)
import Tools.Readability (getFaviconUrlFromUrl, loadReadabilityArticleFromUrl)
import Utility (bug)

--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------

data Page = Page
  { abstract :: Doc
  , kids :: Array Doc
  }

derive instance Generic Page _

instance EncodeJson Page where
  encodeJson x = genericEncodeJson x

instance DecodeJson Page where
  decodeJson x = genericDecodeJson x

--------------------------------------------------------------------------------
-- Doc
--------------------------------------------------------------------------------

data Doc
  = String String
  | Group Group
  | ExternalLink ExternalLink

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
-- ExternalLink
--------------------------------------------------------------------------------

type ExternalLink =
  { href :: String
  , mb_label :: Maybe Doc
  , mb_abstract :: Maybe String
  , mb_icon_src :: Maybe String
  }

--------------------------------------------------------------------------------
-- DocId 
--------------------------------------------------------------------------------

newtype DocId = DocId String

derive newtype instance Show DocId

derive newtype instance Eq DocId

derive newtype instance Ord DocId

derive newtype instance EncodeJson DocId

derive newtype instance DecodeJson DocId

makeDocId :: String -> DocId
makeDocId str | isValidDocId str = DocId str
makeDocId str = bug $ "invalid DocId: " <> str

isValidDocId :: String -> Boolean
isValidDocId _str = true -- TODO: make sure is valid filename

fromDocIdToJsonFilePath :: DocId -> FilePath
fromDocIdToJsonFilePath (DocId str) = "./docs/docs/" <> str <> ".json"

fromDocIdToString :: DocId -> String
fromDocIdToString (DocId pageId_str) = pageId_str

--------------------------------------------------------------------------------
-- PageId
--------------------------------------------------------------------------------

newtype PageId = PageId String

derive newtype instance Show PageId

derive newtype instance Eq PageId

derive newtype instance Ord PageId

derive newtype instance EncodeJson PageId

derive newtype instance DecodeJson PageId

makePageId :: String -> PageId
makePageId str | isValidPageId str = PageId str
makePageId str = bug $ "invalid PageId: " <> str

makePageId' :: String -> Maybe PageId
makePageId' str = do
  guard $ isValidPageId str
  pure $ PageId str

isValidPageId :: String -> Boolean
isValidPageId _str = true -- TODO: make sure is valid filename

fromPageIdToJsonFilePath :: PageId -> FilePath
fromPageIdToJsonFilePath (PageId str) = "./docs/pages/" <> str <> ".json"

fromPageIdToJsonUrl :: PageId -> String
fromPageIdToJsonUrl (PageId str) = "pages/" <> str <> ".json"

fromPageIdToString :: PageId -> String
fromPageIdToString (PageId pageId_str) = pageId_str
