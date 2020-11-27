module RocketLeague.AttributeMapping where

import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  } deriving (Eq, Show)

instance FromBytes.FromBytes AttributeMapping where
  fromBytes = Decoder.label "AttributeMapping" $ do
    objectId <- Decoder.label "objectId" FromBytes.fromBytes
    streamId <- Decoder.label "streamId" FromBytes.fromBytes
    pure AttributeMapping { objectId, streamId }

instance FromJson.FromJson AttributeMapping where
  fromJson = FromJson.object $ \ object -> do
    objectId <- FromJson.required object "objectId"
    streamId <- FromJson.required object "streamId"
    pure AttributeMapping { objectId, streamId }

instance ToBytes.ToBytes AttributeMapping where
  toBytes header = ToBytes.toBytes (objectId header)
    <> ToBytes.toBytes (streamId header)

instance ToJson.ToJson AttributeMapping where
  toJson header = ToJson.object
    [ ("objectId", ToJson.toJson $ objectId header)
    , ("streamId", ToJson.toJson $ streamId header)
    ]
