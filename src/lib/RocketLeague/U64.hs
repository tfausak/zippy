module RocketLeague.U64 (U64, fromWord64, toWord64, fromBytes) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Zippy.ByteGet as ByteGet

newtype U64
  = U64 Word.Word64
  deriving (Eq, Show)

fromWord64 :: Word.Word64 -> U64
fromWord64 = U64

toWord64 :: U64 -> Word.Word64
toWord64 (U64 x) = x

fromBytes :: ByteGet.ByteGet U64
fromBytes = ByteGet.label "U64" $ do
  x <- ByteGet.take 8
  pure . fromWord64
    $ Bits.shiftL (fromIntegral $ ByteString.index x 0) 0
    + Bits.shiftL (fromIntegral $ ByteString.index x 1) 8
    + Bits.shiftL (fromIntegral $ ByteString.index x 2) 16
    + Bits.shiftL (fromIntegral $ ByteString.index x 3) 24
    + Bits.shiftL (fromIntegral $ ByteString.index x 4) 32
    + Bits.shiftL (fromIntegral $ ByteString.index x 5) 40
    + Bits.shiftL (fromIntegral $ ByteString.index x 6) 48
    + Bits.shiftL (fromIntegral $ ByteString.index x 7) 56
