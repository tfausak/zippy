module RocketLeague.Property.Byte (ByteProperty(..), fromBytes) where

import qualified RocketLeague.Str as Str
import qualified Zippy.ByteGet as ByteGet

data ByteProperty = ByteProperty
  { key :: Str.Str
  , value :: Str.Str
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet ByteProperty
fromBytes = ByteGet.label "ByteProperty" $ do
  key <- ByteGet.label "key" Str.fromBytes
  value <- ByteGet.label "value" Str.fromBytes
  pure ByteProperty { key, value }
