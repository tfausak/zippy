module RocketLeague.Header where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Version as Version
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

data Header = Header
  { version :: Version.Version
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Header
decode = ByteDecoder.label "Header" $ do
  version <- ByteDecoder.label "version" Version.decode
  pure Header { version }

encode :: Header -> Builder.Builder
encode header = Version.encode (version header)

fromJson :: JsonDecoder.JsonDecoder Header
fromJson = JsonDecoder.object $ \ object -> do
  version <- JsonDecoder.required object "version" Version.fromJson
  pure Header { version }

toJson :: Header -> Json.Json
toJson header = Json.object
  [ ("version", Version.toJson $ version header)
  ]
