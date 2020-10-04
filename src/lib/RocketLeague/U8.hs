module RocketLeague.U8 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

newtype U8 = U8
  { value :: Word.Word8
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U8
decode = ByteDecoder.label "U8" (do
  theValue <- ByteDecoder.word8
  pure U8 { value = theValue })

encode :: U8 -> Builder.Builder
encode = error "U8/encode"

fromJson :: Json.Json -> Either String U8
fromJson _ = Left "U8/fromJson"

toJson :: U8 -> Json.Json
toJson = error "U8/toJson"
