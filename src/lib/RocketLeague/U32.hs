module RocketLeague.U32 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

newtype U32 = U32
  { value :: Word.Word32
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U32
decode = fail "U32/decode"

encode :: U32 -> Builder.Builder
encode = error "U32/encode"

fromJson :: Json.Json -> Either String U32
fromJson _ = Left "U32/fromJson"

toJson :: U32 -> Json.Json
toJson = error "U32/toJson"
