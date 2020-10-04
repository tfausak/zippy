module RocketLeague.Header where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.Version as Version
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

data Header = Header
  { version :: Version.Version
  } deriving (Eq, Show)

instance FromJson.FromJson Header where
  fromJson = JsonDecoder.object $ \ object -> do
    version <- JsonDecoder.required object "version" FromJson.fromJson
    pure Header { version }

instance ToJson.ToJson Header where
  toJson header = Json.object
    [ ("version", ToJson.toJson $ version header)
    ]

decode :: ByteDecoder.ByteDecoder Header
decode = ByteDecoder.label "Header" $ do
  version <- ByteDecoder.label "version" Version.decode
  pure Header { version }

encode :: Header -> Builder.Builder
encode header = Version.encode (version header)
