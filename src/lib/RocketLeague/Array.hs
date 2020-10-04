module RocketLeague.Array where

import qualified Data.Word as Word
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.List as List

newtype Array a = Array
  { value :: List.List a
  } deriving (Eq, Show)

instance FromBytes.FromBytes a => FromBytes.FromBytes (Array a) where
  fromBytes = do
    size <- FromBytes.fromBytes
    fromBytesWith (U32.value size) 0 List.Empty

instance FromJson.FromJson a => FromJson.FromJson (Array a) where
  fromJson = fmap Array FromJson.fromJson

instance ToBytes.ToBytes a => ToBytes.ToBytes (Array a) where
  toBytes x =
    ToBytes.toBytes (Convert.intToWord32 (length (value x)))
    <> ToBytes.toBytes (value x)

instance ToJson.ToJson a => ToJson.ToJson (Array a) where
  toJson = ToJson.toJson . value

fromBytesWith
  :: FromBytes.FromBytes a
  => Word.Word32
  -> Word.Word32
  -> List.List a
  -> ByteDecoder.ByteDecoder (Array a)
fromBytesWith size index list =
  if index >= size
  then pure . Array $ List.reverse list
  else do
    item <- FromBytes.fromBytes
    fromBytesWith size (index + 1) $ List.Node item list
