module RocketLeague.Property (Property(..), fromBytes) where

import qualified RocketLeague.Property.Array as ArrayProperty
import qualified RocketLeague.Property.Bool as BoolProperty
import qualified RocketLeague.Property.Byte as ByteProperty
import qualified RocketLeague.Property.Float as FloatProperty
import qualified RocketLeague.Property.Int as IntProperty
import qualified RocketLeague.Property.Name as NameProperty
import qualified RocketLeague.Property.QWord as QWordProperty
import qualified RocketLeague.Property.Str as StrProperty
import qualified RocketLeague.Str as Str
import qualified RocketLeague.U64 as U64
import qualified Zippy.ByteGet as ByteGet
import qualified Exception.UnknownProperty as UnknownProperty

data Property
  = Array (ArrayProperty.ArrayProperty Property)
  | Bool BoolProperty.BoolProperty
  | Byte ByteProperty.ByteProperty
  | Float FloatProperty.FloatProperty
  | Int IntProperty.IntProperty
  | Name NameProperty.NameProperty
  | QWord QWordProperty.QWordProperty
  | Str StrProperty.StrProperty
  deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Property
fromBytes = ByteGet.label "Property" $ do
  tag <- ByteGet.label "tag" Str.fromBytes
  _size <- ByteGet.label "size" U64.fromBytes
  case Str.toString tag of
    "ArrayProperty" -> Array <$> ArrayProperty.fromBytes fromBytes
    "BoolProperty" -> fmap Bool BoolProperty.fromBytes
    "ByteProperty" -> fmap Byte ByteProperty.fromBytes
    "FloatProperty" -> fmap Float FloatProperty.fromBytes
    "IntProperty" -> fmap Int IntProperty.fromBytes
    "NameProperty" -> fmap Name NameProperty.fromBytes
    "QWordProperty" -> fmap QWord QWordProperty.fromBytes
    "StrProperty" -> fmap Str StrProperty.fromBytes
    _ -> ByteGet.throw $ UnknownProperty.UnknownProperty tag
