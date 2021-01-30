module RocketLeague.Property.Float (FloatProperty, fromF32, toF32, fromBytes) where

import qualified RocketLeague.F32 as F32
import qualified Zippy.ByteGet as ByteGet

newtype FloatProperty
  = FloatProperty F32.F32
  deriving (Eq, Show)

fromF32 :: F32.F32 -> FloatProperty
fromF32 = FloatProperty

toF32 :: FloatProperty -> F32.F32
toF32 (FloatProperty x) = x

fromBytes :: ByteGet.ByteGet FloatProperty
fromBytes = ByteGet.label "FloatProperty" $ fmap fromF32 F32.fromBytes
