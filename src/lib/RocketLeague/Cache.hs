module RocketLeague.Cache where

import qualified RocketLeague.Array as Array
import qualified RocketLeague.AttributeMapping as AttributeMapping
import qualified RocketLeague.U32 as U32
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: Array.Array AttributeMapping.AttributeMapping
  } deriving (Eq, Show)

instance FromBytes.FromBytes Cache where
  fromBytes = Decoder.label "Cache" $ do
    classId <- Decoder.label "classId" FromBytes.fromBytes
    parentCacheId <- Decoder.label "parentCacheId" FromBytes.fromBytes
    cacheId <- Decoder.label "cacheId" FromBytes.fromBytes
    attributeMappings <- Decoder.label "attributeMappings" FromBytes.fromBytes
    pure Cache { classId, parentCacheId, cacheId, attributeMappings }

instance FromJson.FromJson Cache where
  fromJson = FromJson.object $ \ object -> do
    classId <- FromJson.required object "classId"
    parentCacheId <- FromJson.required object "parentCacheId"
    cacheId <- FromJson.required object "cacheId"
    attributeMappings <- FromJson.required object "attributeMappings"
    pure Cache { classId, parentCacheId, cacheId, attributeMappings }

instance ToBytes.ToBytes Cache where
  toBytes header = ToBytes.toBytes (classId header)
    <> ToBytes.toBytes (parentCacheId header)
    <> ToBytes.toBytes (cacheId header)
    <> ToBytes.toBytes (attributeMappings header)

instance ToJson.ToJson Cache where
  toJson header = ToJson.object
    [ ("classId", ToJson.toJson $ classId header)
    , ("parentCacheId", ToJson.toJson $ parentCacheId header)
    , ("cacheId", ToJson.toJson $ cacheId header)
    , ("attributeMappings", ToJson.toJson $ attributeMappings header)
    ]
