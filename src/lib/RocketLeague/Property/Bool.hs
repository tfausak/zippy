module RocketLeague.Property.Bool (BoolProperty, fromBoolean, toBoolean, fromBytes) where

import qualified RocketLeague.Boolean as Boolean
import qualified Zippy.ByteGet as ByteGet

newtype BoolProperty
  = BoolProperty Boolean.Boolean
  deriving (Eq, Show)

fromBoolean :: Boolean.Boolean -> BoolProperty
fromBoolean = BoolProperty

toBoolean :: BoolProperty -> Boolean.Boolean
toBoolean (BoolProperty x) = x

fromBytes :: ByteGet.ByteGet BoolProperty
fromBytes = ByteGet.label "BoolProperty" $ fmap fromBoolean Boolean.fromBytes
