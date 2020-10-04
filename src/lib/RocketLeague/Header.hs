module RocketLeague.Header where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Version as Version
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

data Header = Header
  { version :: Version.Version
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Header
decode = fail "Header/decode"

encode :: Header -> Builder.Builder
encode = error "Header/encode"

fromJson :: Json.Json -> Either String Header
fromJson _ = Left "Header/fromJson"

toJson :: Header -> Json.Json
toJson = error "Header/toJson"
