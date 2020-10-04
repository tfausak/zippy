module RocketLeague.U32 where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified RocketLeague.U16 as U16
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json

newtype U32 = U32
  { value :: Word.Word32
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder U32
decode = ByteDecoder.label "U32" (do
  lo <- U16.decode
  hi <- U16.decode
  pure (fromU16s lo hi))

fromU16s :: U16.U16 -> U16.U16 -> U32
fromU16s lo hi = U32 $
  Bits.shift (Convert.word16ToWord32 (U16.value hi)) 16 Bits..|.
  Convert.word16ToWord32 (U16.value lo)

encode :: U32 -> Builder.Builder
encode = Builder.word32LE . value

fromJson :: JsonDecoder.JsonDecoder U32
fromJson = JsonDecoder.number (pure . U32 . round)

toJson :: U32 -> Json.Json
toJson = Json.Number . Convert.word32ToDouble . value
