module RocketLeague.Cache (Cache(..), fromBytes) where

import qualified RocketLeague.AttributeMapping as AttributeMapping
import qualified RocketLeague.Vec as Vec
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Cache = Cache
  { classId :: U32.U32
  , parentCacheId :: U32.U32
  , cacheId :: U32.U32
  , attributeMappings :: Vec.Vec AttributeMapping.AttributeMapping
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Cache
fromBytes = ByteGet.label "Cache" $ do
  classId <- ByteGet.label "classId" U32.fromBytes
  parentCacheId <- ByteGet.label "parentCacheId" U32.fromBytes
  cacheId <- ByteGet.label "cacheId" U32.fromBytes
  attributeMappings <- ByteGet.label "attributeMappings" $ Vec.fromBytes AttributeMapping.fromBytes
  pure Cache { classId, parentCacheId, cacheId, attributeMappings }
