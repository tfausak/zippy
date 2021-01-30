module RocketLeague.F32 (F32, fromFloat, toFloat, fromBytes, fromBits) where

import qualified GHC.Float as Float
import qualified RocketLeague.U32 as U32
import qualified Zippy.BitGet as BitGet
import qualified Zippy.ByteGet as ByteGet

newtype F32
  = F32 Float
  deriving (Eq, Show)

fromFloat :: Float -> F32
fromFloat = F32

toFloat :: F32 -> Float
toFloat (F32 x) = x

fromBytes :: ByteGet.ByteGet F32
fromBytes = ByteGet.label "F32" $ do
  x <- U32.fromBytes
  pure . fromFloat . Float.castWord32ToFloat $ U32.toWord32 x

fromBits :: BitGet.BitGet F32
fromBits = BitGet.label "F32" $ do
  x <- U32.fromBits
  pure . fromFloat . Float.castWord32ToFloat $ U32.toWord32 x
