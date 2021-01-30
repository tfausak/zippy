module RocketLeague.CompressedWord (CompressedWord, fromWord, toWord, fromBits) where

import qualified Data.Bits as Bits
import qualified Zippy.BitGet as BitGet
import qualified RocketLeague.Context as Context

newtype CompressedWord
  = CompressedWord Word
  deriving (Eq, Show)

fromWord :: Word -> CompressedWord
fromWord = CompressedWord

toWord :: CompressedWord -> Word
toWord (CompressedWord x) = x

fromBits :: Context.Context -> BitGet.BitGet CompressedWord
fromBits context = BitGet.label "CompressedWord" $ fromBitsWith context 0 0

fromBitsWith :: Context.Context -> Int -> Word -> BitGet.BitGet CompressedWord
fromBitsWith context bit acc =
  if bit < Context.numBits context
    then do
      x <- BitGet.bool
      fromBitsWith context (bit + 1) (if x then acc + Bits.bit bit else acc)
    else do
      let next = acc + Bits.bit bit
      if fromIntegral next < Context.maxChannels context
        then do
          x <- BitGet.bool
          pure . fromWord $ if x then next else acc
        else pure $ fromWord acc
