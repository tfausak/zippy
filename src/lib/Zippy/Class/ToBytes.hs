module Zippy.Class.ToBytes where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Numeric
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

class ToBytes a where
  toBytes :: a -> Builder.Builder

instance ToBytes Bool where
  toBytes = Builder.word8 . Bool.bool 0x00 0x01

instance ToBytes Float where
  toBytes = Builder.floatLE

instance ToBytes Int.Int8 where
  toBytes = Builder.int8

instance ToBytes Int.Int16 where
  toBytes = Builder.int16LE

instance ToBytes Int.Int32 where
  toBytes = Builder.int32LE

instance ToBytes Json.Json where
  toBytes = encodeJson

instance ToBytes a => ToBytes (List.List a) where
  toBytes = foldMap toBytes

instance ToBytes a => ToBytes (Option.Option a) where
  toBytes = Option.option mempty toBytes

instance (ToBytes a, ToBytes b) => ToBytes (Pair.Pair a b) where
  toBytes (Pair.Pair x y) = toBytes x <> toBytes y

instance ToBytes Word.Word8 where
  toBytes = Builder.word8

instance ToBytes Word.Word16 where
  toBytes = Builder.word16LE

instance ToBytes Word.Word32 where
  toBytes = Builder.word32LE

instance ToBytes Word.Word64 where
  toBytes = Builder.word64LE

-------------------------------------------------------------------------------

encodeJson :: Json.Json -> Builder.Builder
encodeJson json = case json of
  Json.Null -> encodeNull
  Json.Boolean x -> encodeBoolean x
  Json.Number x -> encodeNumber x
  Json.String x -> encodeString x
  Json.Array x -> encodeArray x
  Json.Object x -> encodeObject x

encodeNull :: Builder.Builder
encodeNull = Builder.string7 "null"

encodeBoolean :: Bool -> Builder.Builder
encodeBoolean = Builder.string7 . Bool.bool "false" "true"

encodeNumber :: Double -> Builder.Builder
encodeNumber = Builder.string7 . ($ "") . Numeric.showFFloat Nothing

encodeString :: Text.Text -> Builder.Builder
encodeString x =
  Builder.char7 '"'
  <> Text.encodeUtf8BuilderEscaped
    ( B.condB (== 0x22) (encodeChar '\\' '"')
    . B.condB (== 0x5c) (encodeChar '\\' '\\')
    $ B.liftFixedToBounded B.word8
    ) x
  <> Builder.char7 '"'

encodeChar :: Char -> Char -> B.BoundedPrim a
encodeChar x y = B.liftFixedToBounded $ const (x, y) B.>$< B.char7 B.>*< B.char7

encodeArray :: Json.Array -> Builder.Builder
encodeArray xs = case xs of
  List.Empty -> Builder.string7 "[]"
  List.Node x ys -> Builder.string7 "[ " <> encodeJson x <> encodeArrayHelper ys

encodeArrayHelper :: Json.Array -> Builder.Builder
encodeArrayHelper xs = case xs of
  List.Empty -> Builder.string7 " ]"
  List.Node x ys -> Builder.string7 ",\n" <> encodeJson x <> encodeArrayHelper ys

encodeObject :: Json.Object -> Builder.Builder
encodeObject xs = case xs of
  List.Empty -> Builder.string7 "{}"
  List.Node x ys -> Builder.string7 "{ " <> encodePair x <> encodeObjectHelper ys

encodePair :: Pair.Pair Text.Text Json.Json -> Builder.Builder
encodePair (Pair.Pair k v) = encodeString k <> Builder.string7 ": " <> encodeJson v

encodeObjectHelper :: Json.Object -> Builder.Builder
encodeObjectHelper xs = case xs of
  List.Empty -> Builder.string7 " }"
  List.Node x ys -> Builder.string7 ",\n" <> encodePair x <> encodeObjectHelper ys
