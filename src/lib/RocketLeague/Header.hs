module RocketLeague.Header where

import qualified RocketLeague.Version as Version
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
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

instance ToBytes.ToBytes Header where
  toBytes header = ToBytes.toBytes (version header)

instance ToJson.ToJson Header where
  toJson header = Json.object
    [ ("version", ToJson.toJson $ version header)
    ]

decode :: ByteDecoder.ByteDecoder Header
decode = ByteDecoder.label "Header" $ do
  version <- ByteDecoder.label "version" Version.decode
  pure Header { version }
