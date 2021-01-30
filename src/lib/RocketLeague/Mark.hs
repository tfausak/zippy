module RocketLeague.Mark (Mark(..), fromBytes) where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Mark = Mark
  { value :: Str.Str
  , frame :: U32.U32
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Mark
fromBytes = ByteGet.label "Mark" $ do
  value <- ByteGet.label "value" Str.fromBytes
  frame <- ByteGet.label "frame" U32.fromBytes
  pure Mark { value, frame }
