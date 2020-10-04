module Zippy.Convert where

import qualified Control.Monad.Fail as Fail
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Unsafe.Coerce as Unsafe

combine :: Bits.Bits b => (a -> b) -> Int -> a -> a -> b
combine f n x y = f x Bits..|. Bits.shift (f y) n

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac -- TODO: unsafe?

doubleToInt8 :: Fail.MonadFail m => Double -> m Int.Int8
doubleToInt8 = toBounded int8ToDouble

doubleToInt16 :: Fail.MonadFail m => Double -> m Int.Int16
doubleToInt16 = toBounded int16ToDouble

doubleToInt32 :: Fail.MonadFail m => Double -> m Int.Int32
doubleToInt32 = toBounded int32ToDouble

doubleToWord8 :: Fail.MonadFail m => Double -> m Word.Word8
doubleToWord8 = toBounded word8ToDouble

doubleToWord16 :: Fail.MonadFail m => Double -> m Word.Word16
doubleToWord16 = toBounded word16ToDouble

doubleToWord32 :: Fail.MonadFail m => Double -> m Word.Word32
doubleToWord32 = toBounded word32ToDouble

floatToDouble :: Float -> Double
floatToDouble = realToFrac

int8ToDouble :: Int.Int8 -> Double
int8ToDouble = fromIntegral

int8ToInt16 :: Int.Int8 -> Int.Int16
int8ToInt16 = fromIntegral

int16ToDouble :: Int.Int16 -> Double
int16ToDouble = fromIntegral

int16ToInt32 :: Int.Int16 -> Int.Int32
int16ToInt32 = fromIntegral

int32ToDouble :: Int.Int32 -> Double
int32ToDouble = fromIntegral

int32ToInt :: Int.Int32 -> Int
int32ToInt = fromIntegral

intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral -- TODO: unsafe?

intToWord32 :: Int -> Word.Word32
intToWord32 = fromIntegral -- TODO: unsafe?

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

unsafeWord32ToFloat :: Word.Word32 -> Float
unsafeWord32ToFloat = Unsafe.unsafeCoerce

word8ToDouble :: Word.Word8 -> Double
word8ToDouble = fromIntegral

word8ToInt8 :: Word.Word8 -> Int.Int8
word8ToInt8 = fromIntegral

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral

word16ToDouble :: Word.Word16 -> Double
word16ToDouble = fromIntegral

word16ToWord32 :: Word.Word16 -> Word.Word32
word16ToWord32 = fromIntegral

word32ToDouble :: Word.Word32 -> Double
word32ToDouble = fromIntegral

word32ToInt :: Word.Word32 -> Int
word32ToInt = fromIntegral

word32ToWord64 :: Word.Word32 -> Word.Word64
word32ToWord64 = fromIntegral
