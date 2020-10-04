module Zippy.Convert where

import qualified Control.Monad.Fail as Fail
import qualified Data.Bits as Bits
import qualified Data.Word as Word

combine :: Bits.Bits b => (a -> b) -> Int -> a -> a -> b
combine f n x y = f x Bits..|. Bits.shift (f y) n

doubleToWord8 :: Fail.MonadFail m => Double -> m Word.Word8
doubleToWord8 = toBounded word8ToDouble

doubleToWord16 :: Fail.MonadFail m => Double -> m Word.Word16
doubleToWord16 = toBounded word16ToDouble

toBounded
  :: (Fail.MonadFail m, RealFrac a, Show a, Bounded b, Integral b)
  => (b -> a) -> a -> m b
toBounded f x =
  let lo = f minBound in if x < lo then
    fail $ "too small (" <> show x <> " < " <> show lo <> ")"
  else let hi = f maxBound in if x > hi then
    fail $ "too large (" <> show x <> " > " <> show hi <> ")"
  else
    pure $ round x

word8ToDouble :: Word.Word8 -> Double
word8ToDouble = fromIntegral

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral

word16ToDouble :: Word.Word16 -> Double
word16ToDouble = fromIntegral

word16ToWord32 :: Word.Word16 -> Word.Word32
word16ToWord32 = fromIntegral

word32ToDouble :: Word.Word32 -> Double
word32ToDouble = fromIntegral
