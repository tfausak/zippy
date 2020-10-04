module RocketLeague.U16 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

newtype U16 = U16
  { value :: Word.Word16
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U16
decode = fail "U16/decode"

encode :: U16 -> Builder.Builder
encode = error "U16/encode"

fromJson :: Json.Json -> Either String U16
fromJson _ = Left "U16/fromJson"

toJson :: U16 -> Json.Json
toJson = error "U16/toJson"
