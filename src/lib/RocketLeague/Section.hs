module RocketLeague.Section where

import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype Section a = Section
  { value :: a
  } deriving (Eq, Show)

instance FromJson.FromJson a => FromJson.FromJson (Section a) where
  fromJson = fmap Section FromJson.fromJson

instance ToBytes.ToBytes a => ToBytes.ToBytes (Section a) where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson a => ToJson.ToJson (Section a) where
  toJson = ToJson.toJson . value

decode :: ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder (Section a)
decode decodeValue = ByteDecoder.label "Section" $ do
  _size <- ByteDecoder.label "size" U32.decode
  _crc <- ByteDecoder.label "crc" U32.decode
  value <- ByteDecoder.label "value" decodeValue
  pure Section { value }
