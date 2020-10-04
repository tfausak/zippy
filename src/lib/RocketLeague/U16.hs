module RocketLeague.U16 where

import qualified Data.Word as Word
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype U16 = U16
  { value :: Word.Word16
  } deriving (Eq, Show)

instance FromBytes.FromBytes U16 where
  fromBytes = fmap U16 FromBytes.fromBytes

instance FromJson.FromJson U16 where
  fromJson = fmap U16 FromJson.fromJson

instance ToBytes.ToBytes U16 where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson U16 where
  toJson = ToJson.toJson . value
