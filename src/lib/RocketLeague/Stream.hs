module RocketLeague.Stream (Stream, fromByteString, toByteString, fromBytes) where

import qualified Data.ByteString as ByteString
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

newtype Stream
  = Stream ByteString.ByteString
  deriving (Eq, Show)

fromByteString :: ByteString.ByteString -> Stream
fromByteString = Stream

toByteString :: Stream -> ByteString.ByteString
toByteString (Stream x) = x

fromBytes :: ByteGet.ByteGet Stream
fromBytes = ByteGet.label "Stream" $ do
  size <- ByteGet.label "size" U32.fromBytes
  x <- ByteGet.take $ U32.toInt size
  pure $ fromByteString x
