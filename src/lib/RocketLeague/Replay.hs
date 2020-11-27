module RocketLeague.Replay where

import qualified RocketLeague.Content as Content
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Section as Section
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Replay frames = Replay
  { header :: Section.Section Header.Header
  , content :: Section.Section (Content.Content frames)
  } deriving (Eq, Show)

instance FromBytes.FromBytes frames => FromBytes.FromBytes (Replay frames) where
  fromBytes = Decoder.label "Replay" $ do
    header <- Decoder.label "header" FromBytes.fromBytes
    content <- Decoder.label "content" FromBytes.fromBytes
    pure Replay { header, content }

instance FromJson.FromJson frames => FromJson.FromJson (Replay frames) where
  fromJson = FromJson.object $ \ object -> do
    header <- FromJson.required object "header"
    content <- FromJson.required object "content"
    pure Replay { header, content }

instance ToBytes.ToBytes frames => ToBytes.ToBytes (Replay frames) where
  toBytes replay = ToBytes.toBytes (header replay)
    <> ToBytes.toBytes (content replay)

instance ToJson.ToJson frames => ToJson.ToJson (Replay frames) where
  toJson replay = ToJson.object
    [ ("header", ToJson.toJson $ header replay)
    , ("content", ToJson.toJson $ content replay)
    ]
