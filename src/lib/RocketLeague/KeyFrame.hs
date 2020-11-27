module RocketLeague.KeyFrame where

import qualified RocketLeague.U32 as U32
import qualified RocketLeague.F32 as F32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data KeyFrame = KeyFrame
  { time :: F32.F32
  , frame :: U32.U32
  , position :: U32.U32
  } deriving (Eq, Show)

instance FromBytes.FromBytes KeyFrame where
  fromBytes = Decoder.label "KeyFrame" $ do
    time <- Decoder.label "time" FromBytes.fromBytes
    frame <- Decoder.label "frame" FromBytes.fromBytes
    position <- Decoder.label "position" FromBytes.fromBytes
    pure KeyFrame { time, frame, position }

instance FromJson.FromJson KeyFrame where
  fromJson = FromJson.object $ \ object -> do
    time <- FromJson.required object "time"
    frame <- FromJson.required object "frame"
    position <- FromJson.required object "position"
    pure KeyFrame { time, frame, position }

instance ToBytes.ToBytes KeyFrame where
  toBytes header = ToBytes.toBytes (time header)
    <> ToBytes.toBytes (frame header)
    <> ToBytes.toBytes (position header)

instance ToJson.ToJson KeyFrame where
  toJson header = ToJson.object
    [ ("time", ToJson.toJson $ time header)
    , ("frame", ToJson.toJson $ frame header)
    , ("position", ToJson.toJson $ position header)
    ]
