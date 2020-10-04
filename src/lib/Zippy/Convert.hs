module Zippy.Convert where

import qualified Data.Word as Word

word8ToDouble :: Word.Word8 -> Double
word8ToDouble = fromIntegral
