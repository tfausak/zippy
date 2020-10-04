module RocketLeague.Version where

import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Option as Option

data Version = Version
  { major :: U32.U32
  , minor :: U32.U32
  , patch :: Option.Option U32.U32
  } deriving (Eq, Show)

instance FromBytes.FromBytes Version where
  fromBytes = ByteDecoder.label "Version" $ do
    major <- ByteDecoder.label "major" FromBytes.fromBytes
    minor <- ByteDecoder.label "minor" FromBytes.fromBytes
    patch <- ByteDecoder.label "patch" $
      if U32.value major >= 868 && U32.value minor >= 18
      then fmap Option.Some FromBytes.fromBytes
      else pure Option.None
    pure Version { major, minor, patch }

instance FromJson.FromJson Version where
  fromJson = FromJson.object $ \ object -> do
    major <- FromJson.required object "major"
    minor <- FromJson.required object "minor"
    patch <- FromJson.optional object "patch"
    pure Version { major, minor, patch }

instance ToBytes.ToBytes Version where
  toBytes version = ToBytes.toBytes (major version)
    <> ToBytes.toBytes (minor version)
    <> ToBytes.toBytes (patch version)

instance ToJson.ToJson Version where
  toJson version = ToJson.object
    [ ("major", ToJson.toJson $ major version)
    , ("minor", ToJson.toJson $ minor version)
    , ("patch", ToJson.toJson $ patch version)
    ]
