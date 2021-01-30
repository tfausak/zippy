module RocketLeague.Vec (Vec, fromArray, toArray, fromBytes, fromBits) where

import qualified Data.Array as Array
import qualified RocketLeague.U32 as U32
import qualified Zippy.BitGet as BitGet
import qualified Zippy.ByteGet as ByteGet

newtype Vec a
  = Vec (Array.Array Int a)
  deriving (Eq, Show)

fromArray :: Array.Array Int a -> Vec a
fromArray = Vec

toArray :: Vec a -> Array.Array Int a
toArray (Vec x) = x

fromBytes :: ByteGet.ByteGet a -> ByteGet.ByteGet (Vec a)
fromBytes getItem = ByteGet.label "Vec" $ do
  size <- ByteGet.label "size" U32.fromBytes
  fromBytesWith getItem (U32.toInt size - 1) 0 []

fromBytesWith
  :: ByteGet.ByteGet a
  -> Int
  -> Int
  -> [(Int, a)]
  -> ByteGet.ByteGet (Vec a)
fromBytesWith getItem limit index items =
  if index > limit
    then pure . fromArray $ Array.array (0, limit) items
    else do
      item <- ByteGet.label (show index) getItem
      fromBytesWith getItem limit (index + 1) $ (index, item) : items

fromBits :: BitGet.BitGet a -> BitGet.BitGet (Vec a)
fromBits getItem = BitGet.label "Vec" $ fromBitsWith getItem 0 []

fromBitsWith :: BitGet.BitGet a -> Int -> [(Int, a)] -> BitGet.BitGet (Vec a)
fromBitsWith getItem index items = do
  hasItem <- BitGet.bool
  if hasItem
    then do
      item <- BitGet.label (show index) getItem
      fromBitsWith getItem (index + 1) $ (index, item) : items
    else pure . fromArray $ Array.array (0, index - 1) items
