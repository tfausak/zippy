module RocketLeague.Section (Section(..), fromBytes) where

import qualified Data.ByteString as ByteString
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Section = Section
  { size :: U32.U32
  , crc :: U32.U32
  , value :: ByteString.ByteString
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Section
fromBytes = ByteGet.label "Section" $ do
  size <- ByteGet.label "size" U32.fromBytes
  crc <- ByteGet.label "crc" U32.fromBytes
  value <- ByteGet.label "value" . ByteGet.take $ U32.toInt size
  pure Section { size, crc, value }
