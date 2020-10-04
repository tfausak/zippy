module RocketLeague.U16 where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified RocketLeague.U8 as U8
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json

newtype U16 = U16
  { value :: Word.Word16
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U16
decode = ByteDecoder.label "U16" (do
  lo <- U8.decode
  hi <- U8.decode
  pure (fromU8s lo hi))

encode :: U16 -> Builder.Builder
encode = error "U16/encode"

fromJson :: Json.Json -> Either String U16
fromJson _ = Left "U16/fromJson"

fromU8s :: U8.U8 -> U8.U8 -> U16
fromU8s lo hi = U16 $
  Bits.shift (Convert.word8ToWord16 (U8.value hi)) 8 Bits..|.
  Convert.word8ToWord16 (U8.value lo)

toJson :: U16 -> Json.Json
toJson = error "U16/toJson"
