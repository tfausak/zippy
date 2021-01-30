module RocketLeague.Property.Name (NameProperty, fromStr, toStr, fromBytes) where

import qualified RocketLeague.Str as Str
import qualified Zippy.ByteGet as ByteGet

newtype NameProperty
  = NameProperty Str.Str
  deriving (Eq, Show)

fromStr :: Str.Str -> NameProperty
fromStr = NameProperty

toStr :: NameProperty -> Str.Str
toStr (NameProperty x) = x

fromBytes :: ByteGet.ByteGet NameProperty
fromBytes = ByteGet.label "NameProperty" $ fmap fromStr Str.fromBytes
