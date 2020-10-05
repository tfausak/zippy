module RocketLeague.Mark where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Mark = Mark
  { value :: Str.Str
  , frame :: U32.U32
  } deriving (Eq, Show)

instance FromBytes.FromBytes Mark where
  fromBytes = Decoder.label "Mark" $ do
    value <- Decoder.label "value" FromBytes.fromBytes
    frame <- Decoder.label "frame" FromBytes.fromBytes
    pure Mark { value, frame }

instance FromJson.FromJson Mark where
  fromJson = FromJson.object $ \ object -> do
    value <- FromJson.required object "value"
    frame <- FromJson.required object "frame"
    pure Mark { value, frame }

instance ToBytes.ToBytes Mark where
  toBytes header = ToBytes.toBytes (value header)
    <> ToBytes.toBytes (frame header)

instance ToJson.ToJson Mark where
  toJson header = ToJson.object
    [ ("value", ToJson.toJson $ value header)
    , ("frame", ToJson.toJson $ frame header)
    ]
