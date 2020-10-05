module RocketLeague.Stream where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json

newtype Stream = Stream
  { value :: ByteString.ByteString
  } deriving (Eq, Show)

instance FromBytes.FromBytes Stream where
  fromBytes = do
    size <- FromBytes.fromBytes
    fmap Stream . FromBytes.count . Convert.word32ToInt $ U32.value size

instance FromJson.FromJson Stream where
  fromJson = pure $ Stream ByteString.empty -- TODO

instance ToBytes.ToBytes Stream where
  toBytes x = (ToBytes.toBytes . U32.U32 . Convert.unsafeIntToWord32 . ByteString.length $ value x)
    <> Builder.byteString (value x)

instance ToJson.ToJson Stream where
  toJson = const Json.Null
