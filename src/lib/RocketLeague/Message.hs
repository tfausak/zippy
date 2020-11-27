module RocketLeague.Message where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Message = Message
  { frame :: U32.U32
  , name :: Str.Str
  , value :: Str.Str
  } deriving (Eq, Show)

instance FromBytes.FromBytes Message where
  fromBytes = Decoder.label "Message" $ do
    frame <- Decoder.label "frame" FromBytes.fromBytes
    name <- Decoder.label "name" FromBytes.fromBytes
    value <- Decoder.label "value" FromBytes.fromBytes
    pure Message { frame, name, value }

instance FromJson.FromJson Message where
  fromJson = FromJson.object $ \ object -> do
    frame <- FromJson.required object "frame"
    name <- FromJson.required object "name"
    value <- FromJson.required object "value"
    pure Message { frame, name, value }

instance ToBytes.ToBytes Message where
  toBytes header = ToBytes.toBytes (frame header)
    <> ToBytes.toBytes (name header)
    <> ToBytes.toBytes (value header)

instance ToJson.ToJson Message where
  toJson header = ToJson.object
    [ ("frame", ToJson.toJson $ frame header)
    , ("name", ToJson.toJson $ name header)
    , ("value", ToJson.toJson $ value header)
    ]
