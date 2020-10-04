module RocketLeague.Replay where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Section as Section
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

data Replay = Replay
  { header :: Section.Section Header.Header
  } deriving (Eq, Show)

instance FromJson.FromJson Replay where
  fromJson = JsonDecoder.object $ \ object -> do
    header <- JsonDecoder.required object "header" FromJson.fromJson
    pure Replay { header }

decode :: ByteDecoder.ByteDecoder Replay
decode = ByteDecoder.label "Replay" $ do
  header <- ByteDecoder.label "header" $ Section.decode Header.decode
  pure Replay { header }

encode :: Replay -> Builder.Builder
encode replay = Section.encode Header.encode (header replay)

toJson :: Replay -> Json.Json
toJson replay = Json.object
  [ ("header", Section.toJson Header.toJson $ header replay)
  ]
