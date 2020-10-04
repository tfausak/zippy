module RocketLeague.U8 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Convert as Convert
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

newtype U8 = U8
  { value :: Word.Word8
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U8
decode = ByteDecoder.label "U8" $ fmap U8 ByteDecoder.word8

encode :: U8 -> Builder.Builder
encode = Builder.word8 . value

fromJson :: JsonDecoder.JsonDecoder U8
fromJson = JsonDecoder.number $ fmap U8 . Convert.doubleToWord8

toJson :: U8 -> Json.Json
toJson = Json.Number . Convert.word8ToDouble . value
