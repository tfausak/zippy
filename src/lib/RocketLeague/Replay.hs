module RocketLeague.Replay where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Header as Header
import qualified RocketLeague.Section as Section
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

data Replay = Replay
  { header :: Section.Section Header.Header
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Replay
decode = ByteDecoder.label "Replay" (do
  theHeader <- ByteDecoder.label "header" (Section.decode Header.decode)
  pure Replay { header = theHeader })

encode :: Replay -> Builder.Builder
encode = error "Replay/encode"

fromJson :: Json.Json -> Either String Replay
fromJson _ = Left "Replay/fromJson"

toJson :: Replay -> Json.Json
toJson = error "Replay/toJson"
