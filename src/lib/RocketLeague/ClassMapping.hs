module RocketLeague.ClassMapping where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data ClassMapping = ClassMapping
  { name :: Str.Str
  , streamId :: U32.U32
  } deriving (Eq, Show)

instance FromBytes.FromBytes ClassMapping where
  fromBytes = Decoder.label "ClassMapping" $ do
    name <- Decoder.label "name" FromBytes.fromBytes
    streamId <- Decoder.label "streamId" FromBytes.fromBytes
    pure ClassMapping { name, streamId }

instance FromJson.FromJson ClassMapping where
  fromJson = FromJson.object $ \ object -> do
    name <- FromJson.required object "name"
    streamId <- FromJson.required object "streamId"
    pure ClassMapping { name, streamId }

instance ToBytes.ToBytes ClassMapping where
  toBytes header = ToBytes.toBytes (name header)
    <> ToBytes.toBytes (streamId header)

instance ToJson.ToJson ClassMapping where
  toJson header = ToJson.object
    [ ("name", ToJson.toJson $ name header)
    , ("streamId", ToJson.toJson $ streamId header)
    ]
