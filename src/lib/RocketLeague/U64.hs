module RocketLeague.U64 where

import qualified Data.Word as Word
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype U64 = U64
  { value :: Word.Word64
  } deriving (Eq, Show)

instance FromBytes.FromBytes U64 where
  fromBytes = fmap U64 FromBytes.fromBytes

instance FromJson.FromJson U64 where
  fromJson = fmap U64 FromJson.fromJson

instance ToBytes.ToBytes U64 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson U64 where
  toJson = ToJson.toJson . value
