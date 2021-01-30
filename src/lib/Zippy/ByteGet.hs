module Zippy.ByteGet (ByteGet, run, Get.throw, Get.label, Zippy.ByteGet.take) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Ix as Ix
import qualified Exception.OutOfRange as OutOfRange
import qualified Zippy.Get as Get

type ByteGet = Get.Get ByteString.ByteString Identity.Identity

run :: ByteGet a -> ByteString.ByteString -> Either ([String], Exception.SomeException) (ByteString.ByteString, a)
run g = Identity.runIdentity . Get.run g

take :: Int -> ByteGet ByteString.ByteString
take n = Get.label "take" $ do
  s1 <- Get.get
  let r = (0 :: Int, ByteString.length s1)
  Monad.unless (Ix.inRange r n) . Get.throw $ OutOfRange.OutOfRange r n
  let (x, s2) = ByteString.splitAt n s1
  Get.put s2
  pure x
