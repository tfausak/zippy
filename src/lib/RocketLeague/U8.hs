module RocketLeague.U8 where

import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype U8 = U8
  { value :: Word.Word8
  } deriving (Eq, Show)

instance FromJson.FromJson U8 where
  fromJson = fmap U8 FromJson.fromJson

instance ToBytes.ToBytes U8 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson U8 where
  toJson = ToJson.toJson . value

decode :: ByteDecoder.ByteDecoder U8
decode = ByteDecoder.label "U8" $ fmap U8 ByteDecoder.word8
