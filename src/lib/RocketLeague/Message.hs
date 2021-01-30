module RocketLeague.Message (Message(..), fromBytes) where

import qualified RocketLeague.Str as Str
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Message = Message
  { frame :: U32.U32
  , name :: Str.Str
  , value :: Str.Str
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Message
fromBytes = ByteGet.label "Message" $ do
  frame <- ByteGet.label "frame" U32.fromBytes
  name <- ByteGet.label "name" Str.fromBytes
  value <- ByteGet.label "value" Str.fromBytes
  pure Message { frame, name, value }
