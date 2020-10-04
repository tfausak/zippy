module RocketLeague.Content where

import qualified RocketLeague.Array as Array
import qualified RocketLeague.KeyFrame as KeyFrame
import qualified RocketLeague.Str as Str
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Content frames = Content
  { levels :: Array.Array Str.Str
  , keyFrames :: Array.Array KeyFrame.KeyFrame
  , frames :: frames
  -- , messages :: Array.Array Message.Message
  -- , marks :: Array.Array Mark.Mark
  -- , packages :: Array.Array Str.Str
  -- , objects :: Array.Array Str.Str
  -- , names :: Array.Array Str.Str
  -- , classMappings :: Array.Array ClassMapping.ClassMapping
  -- , caches :: Array.Array Cache.Cache
  } deriving (Eq, Show)

instance FromBytes.FromBytes frames => FromBytes.FromBytes (Content frames) where
  fromBytes = Decoder.label "Content" $ do
    levels <- Decoder.label "levels" FromBytes.fromBytes
    keyFrames <- Decoder.label "keyFrames" FromBytes.fromBytes
    frames <- Decoder.label "frames" FromBytes.fromBytes
    pure Content { levels, keyFrames, frames }

instance FromJson.FromJson frames => FromJson.FromJson (Content frames) where
  fromJson = FromJson.object $ \ object -> do
    levels <- FromJson.required object "levels"
    keyFrames <- FromJson.required object "keyFrames"
    frames <- FromJson.required object "frames"
    pure Content { levels, keyFrames, frames }

instance ToBytes.ToBytes frames => ToBytes.ToBytes (Content frames) where
  toBytes header = ToBytes.toBytes (levels header)
    <> ToBytes.toBytes (keyFrames header)
    <> ToBytes.toBytes (frames header)

instance ToJson.ToJson frames => ToJson.ToJson (Content frames) where
  toJson header = ToJson.object
    [ ("levels", ToJson.toJson $ levels header)
    , ("keyFrames", ToJson.toJson $ keyFrames header)
    , ("frames", ToJson.toJson $ frames header)
    ]
