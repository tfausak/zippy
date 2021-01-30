module RocketLeague.Property.Str (StrProperty, fromStr, toStr, fromBytes) where

import qualified RocketLeague.Str as Str
import qualified Zippy.ByteGet as ByteGet

newtype StrProperty
  = StrProperty Str.Str
  deriving (Eq, Show)

fromStr :: Str.Str -> StrProperty
fromStr = StrProperty

toStr :: StrProperty -> Str.Str
toStr (StrProperty x) = x

fromBytes :: ByteGet.ByteGet StrProperty
fromBytes = ByteGet.label "StrProperty" $ fmap fromStr Str.fromBytes
