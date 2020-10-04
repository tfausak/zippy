module RocketLeague.Version where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.Option as Option

data Version = Version
  { major :: U32.U32
  , minor :: U32.U32
  , patch :: Option.Option U32.U32
  } deriving (Eq, Show)

instance FromJson.FromJson Version where
  fromJson = JsonDecoder.object $ \ object -> do
    major <- JsonDecoder.required object "major" FromJson.fromJson
    minor <- JsonDecoder.required object "minor" FromJson.fromJson
    patch <- JsonDecoder.optional object "patch" FromJson.fromJson
    pure Version { major, minor, patch }

instance ToJson.ToJson Version where
  toJson version = Json.object
    [ ("major", ToJson.toJson $ major version)
    , ("minor", ToJson.toJson $ minor version)
    , ("patch", ToJson.toJson $ patch version)
    ]

decode :: ByteDecoder.ByteDecoder Version
decode = ByteDecoder.label "Version" $ do
  major <- ByteDecoder.label "major" U32.decode
  minor <- ByteDecoder.label "minor" U32.decode
  patch <- ByteDecoder.label "patch" $
    if U32.value major >= 868 && U32.value minor >= 18
    then fmap Option.Some U32.decode
    else pure Option.None
  pure Version { major, minor, patch }

encode :: Version -> Builder.Builder
encode version = U32.encode (major version)
  <> U32.encode (minor version)
  <> Option.option mempty U32.encode (patch version)
