module RocketLeague.Section where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

newtype Section a = Section
  { value :: a
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder (Section a)
decode decodeValue = ByteDecoder.label "Section" $ do
  _size <- ByteDecoder.label "size" U32.decode
  _crc <- ByteDecoder.label "crc" U32.decode
  value <- ByteDecoder.label "value" decodeValue
  pure Section { value }

encode :: (a -> Builder.Builder) -> Section a -> Builder.Builder
encode f = f . value

fromJson :: (JsonDecoder.JsonDecoder a) -> JsonDecoder.JsonDecoder (Section a)
fromJson = fmap Section

toJson :: (a -> Json.Json) -> Section a -> Json.Json
toJson f = f . value
