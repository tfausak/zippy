module RocketLeague.Frames where

import qualified RocketLeague.Frame as Frame
import qualified RocketLeague.Replay as Replay
import qualified RocketLeague.Stream as Stream
import qualified Zippy.Type.List as List
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype Frames = Frames
  { value :: List.List Frame.Frame
  } deriving (Eq, Show)

instance FromJson.FromJson Frames where
  fromJson = error "Frames/fromJson"

instance ToBytes.ToBytes Frames where
  toBytes = error "Frames/toBytes"

instance ToJson.ToJson Frames where
  toJson = error "Frames/toJson"

fromBytes :: Replay.Replay Stream.Stream -> FromBytes.ByteDecoder Frames
fromBytes = error "Frames/fromBytes"
