module RocketLeague.U32 where

import qualified Data.Word as Word
import qualified RocketLeague.U16 as U16
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Convert as Convert

newtype U32 = U32
  { value :: Word.Word32
  } deriving (Eq, Show)

instance FromJson.FromJson U32 where
  fromJson = fmap U32 FromJson.fromJson

instance ToBytes.ToBytes U32 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson U32 where
  toJson = ToJson.toJson . value

decode :: ByteDecoder.ByteDecoder U32
decode = ByteDecoder.label "U32" $ do
  lo <- U16.decode
  hi <- U16.decode
  pure . U32 $ Convert.combine (Convert.word16ToWord32 . U16.value) 16 lo hi
