module Zippy.BitGet (BitGet, run, Get.label, bool, word8) where

import qualified Control.Exception as Exception
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Word as Word
import qualified Exception.Empty as Empty
import qualified Zippy.BitString as BitString
import qualified Zippy.Get as Get

type BitGet = Get.Get BitString.BitString Identity.Identity

run :: BitGet a -> BitString.BitString -> Either ([String], Exception.SomeException) (BitString.BitString, a)
run g = Identity.runIdentity . Get.run g

bool :: BitGet Bool
bool = do
  bits <- Get.get
  case ByteString.uncons $ BitString.byteString bits of
    Nothing -> Get.throw Empty.Empty
    Just (x, bytes) -> do
      let index = BitString.index bits
      Get.put $ if index == 7
        then BitString.fromByteString bytes
        else bits { BitString.index = index + 1 }
      pure $ Bits.testBit x index

word8 :: BitGet Word.Word8
word8 = do
  a <- bool
  b <- bool
  c <- bool
  d <- bool
  e <- bool
  f <- bool
  g <- bool
  h <- bool
  pure
    $ (if a then 1 else 0)
    + (if b then 2 else 0)
    + (if c then 4 else 0)
    + (if d then 8 else 0)
    + (if e then 16 else 0)
    + (if f then 32 else 0)
    + (if g then 64 else 0)
    + (if h then 128 else 0)
