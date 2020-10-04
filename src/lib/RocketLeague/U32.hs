module RocketLeague.U32 where

import qualified Data.Word as Word
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype U32 = U32
  { value :: Word.Word32
  } deriving (Eq, Show)

instance FromBytes.FromBytes U32 where
  fromBytes = fmap U32 FromBytes.fromBytes

instance FromJson.FromJson U32 where
  fromJson = fmap U32 FromJson.fromJson

instance ToBytes.ToBytes U32 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson U32 where
  toJson = ToJson.toJson . value
