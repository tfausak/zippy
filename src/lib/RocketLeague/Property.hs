module RocketLeague.Property where

import qualified Data.Text as Text
import qualified RocketLeague.Array as Array
import qualified RocketLeague.Boolean as Boolean
import qualified RocketLeague.Dictionary as Dictionary
import qualified RocketLeague.F32 as F32
import qualified RocketLeague.I32 as I32
import qualified RocketLeague.Str as Str
import qualified RocketLeague.U64 as U64
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.Pair as Pair

data Property
  = Array (Array.Array (Dictionary.Dictionary Property))
  | Bool Boolean.Boolean
  | Byte (Pair.Pair Str.Str Str.Str)
  | Float F32.F32
  | Int I32.I32
  | Name Str.Str
  | QWord U64.U64
  | Str Str.Str
  deriving (Eq, Show)

instance FromBytes.FromBytes Property where
  fromBytes = do
    kind <- FromBytes.fromBytes
    size <- FromBytes.fromBytes
    fromBytesWith kind size

instance FromJson.FromJson Property where
  fromJson = FromJson.object $ \ object -> do
    tag <- FromJson.required object "tag"
    case Text.unpack tag of
      "Array" -> fmap Array $ FromJson.required object "val"
      "Bool" -> fmap Bool $ FromJson.required object "val"
      "Byte" -> fmap Byte $ FromJson.required object "val"
      "Float" -> fmap Float $ FromJson.required object "val"
      "Int" -> fmap Int $ FromJson.required object "val"
      "Name" -> fmap Name $ FromJson.required object "val"
      "QWord" -> fmap QWord $ FromJson.required object "val"
      "Str" -> fmap Str $ FromJson.required object "val"
      _ -> fail $ "unknown tag: " <> show tag

instance ToBytes.ToBytes Property where
  toBytes x = (ToBytes.toBytes . Str.Str $ toTag x)
    <> ToBytes.toBytes (U64.U64 0)
    <> case x of
      Array y -> ToBytes.toBytes y
      Bool y -> ToBytes.toBytes y
      Byte y -> ToBytes.toBytes y
      Float y -> ToBytes.toBytes y
      Int y -> ToBytes.toBytes y
      Name y -> ToBytes.toBytes y
      QWord y -> ToBytes.toBytes y
      Str y -> ToBytes.toBytes y

instance ToJson.ToJson Property where
  toJson x = case x of
    Array y -> tagged x y
    Bool y -> tagged x y
    Byte y -> tagged x y
    Float y -> tagged x y
    Int y -> tagged x y
    Name y -> tagged x y
    QWord y -> tagged x y
    Str y -> tagged x y

tagged :: ToJson.ToJson a => Property -> a -> Json.Json
tagged x y = ToJson.object
  [ ("tag", ToJson.toJson $ toTag x)
  , ("val", ToJson.toJson y)
  ]

toTag :: Property -> Text.Text
toTag x = Text.pack $ case x of
  Array _ -> "Array"
  Bool _ -> "Bool"
  Byte _ -> "Byte"
  Float _ -> "Float"
  Int _ -> "Int"
  Name _ -> "Name"
  QWord _ -> "QWord"
  Str _ -> "Str"

fromBytesWith :: Str.Str -> U64.U64 -> ByteDecoder.ByteDecoder Property
fromBytesWith kind _ = case Str.toString kind of
  "ArrayProperty" -> fmap Array FromBytes.fromBytes
  "BoolProperty" -> fmap Bool FromBytes.fromBytes
  "ByteProperty" -> do
    k <- FromBytes.fromBytes
    if Str.toString k == "OnlinePlatform_Steam"
      then pure . Byte $ Pair.Pair (Str.fromString "OnlinePlatform") k
      else do
        v <- FromBytes.fromBytes
        pure . Byte $ Pair.Pair k v
  "FloatProperty" -> fmap Float FromBytes.fromBytes
  "IntProperty" -> fmap Int FromBytes.fromBytes
  "NameProperty" -> fmap Name FromBytes.fromBytes
  "QWordProperty" -> fmap QWord FromBytes.fromBytes
  "StrProperty" -> fmap Str FromBytes.fromBytes
  _ -> fail $ "unknown kind: " <> show kind
