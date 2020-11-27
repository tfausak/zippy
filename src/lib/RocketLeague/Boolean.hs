module RocketLeague.Boolean where

import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype Boolean = Boolean
  { value :: Bool
  } deriving (Eq, Show)

instance FromBytes.FromBytes Boolean where
  fromBytes = fmap Boolean FromBytes.fromBytes

instance FromJson.FromJson Boolean where
  fromJson = fmap Boolean FromJson.fromJson

instance ToBytes.ToBytes Boolean where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson Boolean where
  toJson = ToJson.toJson . value
