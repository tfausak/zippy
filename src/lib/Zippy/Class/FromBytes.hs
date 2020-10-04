module Zippy.Class.FromBytes where

import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Convert as Convert

class FromBytes a where
  fromBytes :: ByteDecoder.ByteDecoder a

instance FromBytes Word.Word8 where
  fromBytes = ByteDecoder.word8

instance FromBytes Word.Word16 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word8ToWord16 8 lo hi

instance FromBytes Word.Word32 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word16ToWord32 16 lo hi
