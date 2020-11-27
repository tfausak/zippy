module RocketLeague.F32 where

import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype F32 = F32
  { value :: Float
  } deriving (Eq, Show)

instance FromBytes.FromBytes F32 where
  fromBytes = fmap F32 FromBytes.fromBytes

instance FromJson.FromJson F32 where
  fromJson = fmap F32 FromJson.fromJson

instance ToBytes.ToBytes F32 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson F32 where
  toJson = ToJson.toJson . value
