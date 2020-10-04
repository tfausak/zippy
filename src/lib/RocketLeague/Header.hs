module RocketLeague.Header where

import qualified RocketLeague.Version as Version
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

data Header = Header
  { version :: Version.Version
  } deriving (Eq, Show)

instance FromBytes.FromBytes Header where
  fromBytes = ByteDecoder.label "Header" $ do
    version <- ByteDecoder.label "version" FromBytes.fromBytes
    pure Header { version }

instance FromJson.FromJson Header where
  fromJson = FromJson.object $ \ object -> do
    version <- FromJson.required object "version"
    pure Header { version }

instance ToBytes.ToBytes Header where
  toBytes header = ToBytes.toBytes (version header)

instance ToJson.ToJson Header where
  toJson header = ToJson.object
    [ ("version", ToJson.toJson $ version header)
    ]
