module RocketLeague.Replay where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Section as Section
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

data Replay = Replay
  { header :: Section.Section Header.Header
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Replay
decode = ByteDecoder.label "Replay" (do
  theHeader <- ByteDecoder.label "header" (Section.decode Header.decode)
  pure Replay { header = theHeader })

encode :: Replay -> Builder.Builder
encode replay = Section.encode Header.encode (header replay)

fromJson :: JsonDecoder.JsonDecoder Replay
fromJson = JsonDecoder.object (\ object -> do
  theHeader <- JsonDecoder.required "header" object (Section.fromJson Header.fromJson)
  pure Replay { header = theHeader })

toJson :: Replay -> Json.Json
toJson replay = Json.object
  [ ("header", Section.toJson Header.toJson (header replay))
  ]
