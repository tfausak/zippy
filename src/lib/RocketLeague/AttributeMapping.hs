module RocketLeague.AttributeMapping (AttributeMapping(..), fromBytes) where

import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet AttributeMapping
fromBytes = ByteGet.label "AttributeMapping" $ do
  objectId <- ByteGet.label "objectId" U32.fromBytes
  streamId <- ByteGet.label "streamId" U32.fromBytes
  pure AttributeMapping { objectId, streamId }
