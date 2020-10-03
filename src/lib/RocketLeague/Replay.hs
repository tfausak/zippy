module RocketLeague.Replay where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json

data Replay = Replay
  {
  } deriving (Eq, Show)

decode :: Monad m => Decoder.Decoder ByteString.ByteString u m Replay
decode = pure Replay

encode :: Replay -> Builder.Builder
encode _ = mempty

fromJson :: Json.Json -> Either String Replay
fromJson _ = pure Replay -- TODO

toJson :: Replay -> Json.Json
toJson _ = Json.object []
