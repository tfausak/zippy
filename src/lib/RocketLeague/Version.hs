module RocketLeague.Version where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json

data Version = Version
  { major :: U32.U32
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Version
decode = ByteDecoder.label "Version" $ do
  major <- ByteDecoder.label "major" U32.decode
  pure Version { major }

encode :: Version -> Builder.Builder
encode version = U32.encode (major version)

fromJson :: JsonDecoder.JsonDecoder Version
fromJson = JsonDecoder.object $ \ object -> do
  major <- JsonDecoder.required object "major" U32.fromJson
  pure Version { major }

toJson :: Version -> Json.Json
toJson version = Json.object
  [ ("major", U32.toJson $ major version)
  ]
