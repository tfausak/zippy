module Zippy.Convert where

import qualified Data.Word as Word

word8ToDouble :: Word.Word8 -> Double
word8ToDouble = fromIntegral

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral

word16ToWord32 :: Word.Word16 -> Word.Word32
word16ToWord32 = fromIntegral

word32ToDouble :: Word.Word32 -> Double
word32ToDouble = fromIntegral
