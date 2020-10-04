module RocketLeague.Replay where

import qualified RocketLeague.Header as Header
import qualified RocketLeague.Section as Section
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Replay = Replay
  { header :: Section.Section Header.Header
  } deriving (Eq, Show)

instance FromBytes.FromBytes Replay where
  fromBytes = Decoder.label "Replay" $ do
    header <- Decoder.label "header" FromBytes.fromBytes
    pure Replay { header }

instance FromJson.FromJson Replay where
  fromJson = FromJson.object $ \ object -> do
    header <- FromJson.required object "header"
    pure Replay { header }

instance ToBytes.ToBytes Replay where
  toBytes replay = ToBytes.toBytes (header replay)

instance ToJson.ToJson Replay where
  toJson replay = ToJson.object
    [ ("header", ToJson.toJson $ header replay)
    ]
