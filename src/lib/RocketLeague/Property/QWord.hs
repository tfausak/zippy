module RocketLeague.Property.QWord (QWordProperty, fromU64, toU64, fromBytes) where

import qualified RocketLeague.U64 as U64
import qualified Zippy.ByteGet as ByteGet

newtype QWordProperty
  = QWordProperty U64.U64
  deriving (Eq, Show)

fromU64 :: U64.U64 -> QWordProperty
fromU64 = QWordProperty

toU64 :: QWordProperty -> U64.U64
toU64 (QWordProperty x) = x

fromBytes :: ByteGet.ByteGet QWordProperty
fromBytes = ByteGet.label "QWordProperty" $ fmap fromU64 U64.fromBytes
