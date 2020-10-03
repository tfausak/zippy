module RocketLeague.Replay where

import qualified Data.ByteString as ByteString
import qualified Zippy.Type.Json as Json

data Replay = Replay
  {
  } deriving (Eq, Show)

decode :: ByteString.ByteString -> Either String Replay
decode _ = Right Replay {}

encode :: Replay -> ByteString.ByteString
encode _ = ByteString.empty

fromJson :: Json.Json -> Either String Replay
fromJson _ = Right Replay {}

toJson :: Replay -> Json.Json
toJson _ = Json.Null
