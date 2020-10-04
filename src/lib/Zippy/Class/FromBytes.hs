module Zippy.Class.FromBytes where

import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Pair as Pair

class FromBytes a where
  fromBytes :: ByteDecoder.ByteDecoder a

instance FromBytes Bool where
  fromBytes = fmap (/= (0x00 :: Word.Word8)) fromBytes

instance FromBytes Float where
  fromBytes = fmap Convert.unsafeWord32ToFloat fromBytes

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

instance (FromBytes a, FromBytes b) => FromBytes (Pair.Pair a b) where
  fromBytes = do
    x <- fromBytes
    y <- fromBytes
    pure $ Pair.Pair x y

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

instance FromBytes Word.Word64 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word32ToWord64 32 lo hi
