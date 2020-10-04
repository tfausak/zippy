module RocketLeague.I32 where

import qualified Data.Int as Int
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype I32 = I32
  { value :: Int.Int32
  } deriving (Eq, Show)

instance FromBytes.FromBytes I32 where
  fromBytes = fmap I32 FromBytes.fromBytes

instance FromJson.FromJson I32 where
  fromJson = fmap I32 FromJson.fromJson

instance ToBytes.ToBytes I32 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson I32 where
  toJson = ToJson.toJson . value
