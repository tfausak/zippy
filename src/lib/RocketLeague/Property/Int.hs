module RocketLeague.Property.Int (IntProperty, fromI32, toI32, fromBytes) where

import qualified RocketLeague.I32 as I32
import qualified Zippy.ByteGet as ByteGet

newtype IntProperty
  = IntProperty I32.I32
  deriving (Eq, Show)

fromI32 :: I32.I32 -> IntProperty
fromI32 = IntProperty

toI32 :: IntProperty -> I32.I32
toI32 (IntProperty x) = x

fromBytes :: ByteGet.ByteGet IntProperty
fromBytes = ByteGet.label "IntProperty" $ fmap fromI32 I32.fromBytes
