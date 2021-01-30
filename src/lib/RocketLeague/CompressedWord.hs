module RocketLeague.CompressedWord (CompressedWord, fromWord, toWord, fromBits) where

import qualified Data.Bits as Bits
import qualified Zippy.BitGet as BitGet

newtype CompressedWord
  = CompressedWord Word
  deriving (Eq, Show)

fromWord :: Word -> CompressedWord
fromWord = CompressedWord

toWord :: CompressedWord -> Word
toWord (CompressedWord x) = x

fromBits :: Int -> BitGet.BitGet CompressedWord
fromBits maxChannels = BitGet.label "CompressedWord" $ do
  let numBits = floor (logBase 2 $ fromIntegral maxChannels :: Double) :: Int
  fromBitsWith maxChannels numBits 0 0

fromBitsWith :: Int -> Int -> Int -> Word -> BitGet.BitGet CompressedWord
fromBitsWith maxChannels numBits bit acc =
  if bit < numBits
    then do
      x <- BitGet.bool
      fromBitsWith maxChannels numBits (bit + 1) (if x then acc + Bits.bit bit else acc)
    else do
      let next = acc + Bits.bit bit
      if fromIntegral next < maxChannels
        then do
          x <- BitGet.bool
          pure . fromWord $ if x then next else acc
        else pure $ fromWord acc
