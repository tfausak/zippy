module RocketLeague.ClassMapping (ClassMapping(..), fromBytes) where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data ClassMapping = ClassMapping
  { name :: Str.Str
  , streamId :: U32.U32
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet ClassMapping
fromBytes = ByteGet.label "ClassMapping" $ do
  name <- ByteGet.label "name" Str.fromBytes
  streamId <- ByteGet.label "streamId" U32.fromBytes
  pure ClassMapping { name, streamId }
