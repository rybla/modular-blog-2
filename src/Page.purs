module Page where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Doc (Doc)

data Page = Page
  { abstract :: Doc
  , kids :: Array Doc
  }

derive instance Generic Page _

instance EncodeJson Page where
  encodeJson x = genericEncodeJson x

instance DecodeJson Page where
  decodeJson x = genericDecodeJson x
