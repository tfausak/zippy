module RocketLeague.U32 (U32, fromWord32, toWord32, toInt, fromBytes, fromBits) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Zippy.BitGet as BitGet
import qualified Zippy.ByteGet as ByteGet

newtype U32
  = U32 Word.Word32
  deriving (Eq, Show)

fromWord32 :: Word.Word32 -> U32
fromWord32 = U32

toWord32 :: U32 -> Word.Word32
toWord32 (U32 x) = x

toInt :: U32 -> Int
toInt = fromIntegral . toWord32

fromBytes :: ByteGet.ByteGet U32
fromBytes = ByteGet.label "U32" $ do
  x <- ByteGet.take 4
  pure . fromWord32
    $ Bits.shiftL (fromIntegral $ ByteString.index x 0) 0
    + Bits.shiftL (fromIntegral $ ByteString.index x 1) 8
    + Bits.shiftL (fromIntegral $ ByteString.index x 2) 16
    + Bits.shiftL (fromIntegral $ ByteString.index x 3) 24

fromBits :: BitGet.BitGet U32
fromBits = BitGet.label "U32" $ do
  a <- BitGet.word8
  b <- BitGet.word8
  c <- BitGet.word8
  d <- BitGet.word8
  pure . fromWord32
    $ Bits.shiftL (fromIntegral a) 0
    + Bits.shiftL (fromIntegral b) 8
    + Bits.shiftL (fromIntegral c) 16
    + Bits.shiftL (fromIntegral d) 24
