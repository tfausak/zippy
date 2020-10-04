module Zippy.Class.FromBytes where

import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Convert as Convert

class FromBytes a where
  fromBytes :: ByteDecoder.ByteDecoder a

instance FromBytes Int.Int8 where
  fromBytes = fmap Convert.word8ToInt8 ByteDecoder.word8

instance FromBytes Int.Int16 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.int8ToInt16 8 lo hi

instance FromBytes Int.Int32 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.int16ToInt32 16 lo hi

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
