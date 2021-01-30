module RocketLeague.I32 (I32, fromInt32, toInt32, toInt, fromBytes) where

import qualified Data.Int as Int
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

newtype I32
  = I32 Int.Int32
  deriving (Eq, Show)

fromInt32 :: Int.Int32 -> I32
fromInt32 = I32

toInt32 :: I32 -> Int.Int32
toInt32 (I32 x) = x

toInt :: I32 -> Int
toInt = fromIntegral . toInt32

fromBytes :: ByteGet.ByteGet I32
fromBytes = ByteGet.label "I32" $ do
  x <- U32.fromBytes
  pure . fromInt32 . fromIntegral $ U32.toWord32 x
