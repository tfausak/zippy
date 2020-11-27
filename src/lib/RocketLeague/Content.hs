module RocketLeague.Content where

import qualified RocketLeague.Array as Array
import qualified RocketLeague.Cache as Cache
import qualified RocketLeague.ClassMapping as ClassMapping
import qualified RocketLeague.KeyFrame as KeyFrame
import qualified RocketLeague.Mark as Mark
import qualified RocketLeague.Message as Message
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
  , messages :: Array.Array Message.Message
  , marks :: Array.Array Mark.Mark
  , packages :: Array.Array Str.Str
  , objects :: Array.Array Str.Str
  , names :: Array.Array Str.Str
  , classMappings :: Array.Array ClassMapping.ClassMapping
  , caches :: Array.Array Cache.Cache
  } deriving (Eq, Show)

instance FromBytes.FromBytes frames => FromBytes.FromBytes (Content frames) where
  fromBytes = Decoder.label "Content" $ do
    levels <- Decoder.label "levels" FromBytes.fromBytes
    keyFrames <- Decoder.label "keyFrames" FromBytes.fromBytes
    frames <- Decoder.label "frames" FromBytes.fromBytes
    messages <- Decoder.label "messages" FromBytes.fromBytes
    marks <- Decoder.label "marks" FromBytes.fromBytes
    packages <- Decoder.label "packages" FromBytes.fromBytes
    objects <- Decoder.label "objects" FromBytes.fromBytes
    names <- Decoder.label "names" FromBytes.fromBytes
    classMappings <- Decoder.label "classMappings" FromBytes.fromBytes
    caches <- Decoder.label "caches" FromBytes.fromBytes
    pure Content { levels, keyFrames, frames, messages, marks, packages, objects, names, classMappings, caches }

instance FromJson.FromJson frames => FromJson.FromJson (Content frames) where
  fromJson = FromJson.object $ \ object -> do
    levels <- FromJson.required object "levels"
    keyFrames <- FromJson.required object "keyFrames"
    frames <- FromJson.required object "frames"
    messages <- FromJson.required object "messages"
    marks <- FromJson.required object "marks"
    packages <- FromJson.required object "packages"
    objects <- FromJson.required object "objects"
    names <- FromJson.required object "names"
    classMappings <- FromJson.required object "classMappings"
    caches <- FromJson.required object "caches"
    pure Content { levels, keyFrames, frames, messages, marks, packages, objects, names, classMappings, caches }

instance ToBytes.ToBytes frames => ToBytes.ToBytes (Content frames) where
  toBytes header = ToBytes.toBytes (levels header)
    <> ToBytes.toBytes (keyFrames header)
    <> ToBytes.toBytes (frames header)
    <> ToBytes.toBytes (messages header)
    <> ToBytes.toBytes (marks header)
    <> ToBytes.toBytes (packages header)
    <> ToBytes.toBytes (objects header)
    <> ToBytes.toBytes (names header)
    <> ToBytes.toBytes (classMappings header)
    <> ToBytes.toBytes (caches header)

instance ToJson.ToJson frames => ToJson.ToJson (Content frames) where
  toJson header = ToJson.object
    [ ("levels", ToJson.toJson $ levels header)
    , ("keyFrames", ToJson.toJson $ keyFrames header)
    , ("frames", ToJson.toJson $ frames header)
    , ("messages", ToJson.toJson $ messages header)
    , ("marks", ToJson.toJson $ marks header)
    , ("packages", ToJson.toJson $ packages header)
    , ("objects", ToJson.toJson $ objects header)
    , ("names", ToJson.toJson $ names header)
    , ("classMappings", ToJson.toJson $ classMappings header)
    , ("caches", ToJson.toJson $ caches header)
    ]
