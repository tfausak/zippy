module Zippy.Type.Json where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Bool as Bool
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair

data Json
  = Null
  | Boolean Bool
  | Number Double
  | String Text.Text
  | Array (List.List Json)
  | Object (List.List (Pair.Pair Text.Text Json))
  deriving (Eq, Show)

string :: String -> Json
string = String . Text.pack

array :: [Json] -> Json
array = Array . List.fromList

object :: [(String, Json)] -> Json
object = Object . List.fromList . fmap (Pair.fromTuple . Bifunctor.first Text.pack)

decode :: Monad m => Decoder.Decoder ByteString.ByteString u m Json
decode = pure Null -- TODO

encode :: Json -> Builder.Builder
encode json = case json of
  Null -> encodeNull
  Boolean x -> encodeBoolean x
  Number x -> encodeNumber x
  Array x -> encodeArray x
  Object x -> encodeObject x
  _ -> mempty

encodeNull :: Builder.Builder
encodeNull = Builder.string7 "null"

encodeBoolean :: Bool -> Builder.Builder
encodeBoolean = Builder.string7 . Bool.bool "false" "true"

encodeNumber :: Double -> Builder.Builder
encodeNumber = Builder.doubleDec

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

encodeArray :: List.List Json -> Builder.Builder
encodeArray xs = case xs of
  List.Empty -> Builder.string7 "[]"
  List.Node x ys -> Builder.string7 "[ " <> encode x <> encodeArrayHelper ys

encodeArrayHelper :: List.List Json -> Builder.Builder
encodeArrayHelper xs = case xs of
  List.Empty -> Builder.string7 " ]"
  List.Node x ys -> Builder.string7 ",\n" <> encode x <> encodeArrayHelper ys

encodeObject :: List.List (Pair.Pair Text.Text Json) -> Builder.Builder
encodeObject xs = case xs of
  List.Empty -> Builder.string7 "{}"
  List.Node x ys -> Builder.string7 "{ " <> encodePair x <> encodeObjectHelper ys

encodePair :: Pair.Pair Text.Text Json -> Builder.Builder
encodePair (Pair.Pair k v) = encodeString k <> Builder.string7 ": " <> encode v

encodeObjectHelper :: List.List (Pair.Pair Text.Text Json) -> Builder.Builder
encodeObjectHelper xs = case xs of
  List.Empty -> Builder.string7 " }"
  List.Node x ys -> encodePair x <> Builder.string7 ",\n" <> encodeObjectHelper ys
