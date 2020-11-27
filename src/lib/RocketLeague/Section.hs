module RocketLeague.Section where

import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

newtype Section a = Section
  { value :: a
  } deriving (Eq, Show)

instance FromBytes.FromBytes a => FromBytes.FromBytes (Section a) where
  fromBytes = Decoder.label "Section" $ do
    size <- Decoder.label "size" FromBytes.fromBytes
    crc <- Decoder.label "crc" FromBytes.fromBytes
    checkCrc size crc
    value <- Decoder.label "value" FromBytes.fromBytes
    pure Section { value }

instance FromJson.FromJson a => FromJson.FromJson (Section a) where
  fromJson = fmap Section FromJson.fromJson

instance ToBytes.ToBytes a => ToBytes.ToBytes (Section a) where
  toBytes = ToBytes.toBytes . value

instance ToJson.ToJson a => ToJson.ToJson (Section a) where
  toJson = ToJson.toJson . value

checkCrc :: Applicative m => U32.U32 -> U32.U32 -> m ()
checkCrc _ _ = pure () -- TODO
