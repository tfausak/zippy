module Zippy.Type.Json where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as B
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair

data Json
  = Null
  | Boolean Bool
  | Number Double
  | String Text.Text
  | Array Array
  | Object Object
  deriving (Eq, Show)

type Array = List.List Json

type Object = List.List (Pair.Pair Text.Text Json)

string :: String -> Json
string = String . Text.pack

array :: [Json] -> Json
array = Array . List.fromList

object :: [(String, Json)] -> Json
object = Object . List.fromList . fmap (Pair.fromTuple . Bifunctor.first Text.pack)

decode :: ByteDecoder.ByteDecoder Json
decode = do
  decodeBlankSpaces
  decodeJson

decodeBlankSpaces :: ByteDecoder.ByteDecoder ()
decodeBlankSpaces = Monad.void (ByteDecoder.munch isBlankSpace)

isBlankSpace :: Word.Word8 -> Bool
isBlankSpace byte = case byte of
  0x09 -> True -- horizontal tab
  0x0a -> True -- new line
  0x0d -> True -- carriage ret
  0x20 -> True -- space
  _ -> False

decodeJson :: ByteDecoder.ByteDecoder Json
decodeJson = decodeNull
  Applicative.<|> decodeBoolean
  Applicative.<|> decodeNumber
  Applicative.<|> decodeString
  Applicative.<|> decodeArray
  Applicative.<|> decodeObject

decodeNull :: ByteDecoder.ByteDecoder Json
decodeNull = do
  decodeSymbol "null"
  pure Null

decodeSymbol :: String -> ByteDecoder.ByteDecoder ()
decodeSymbol x = do
  Monad.void (mapM_ decodeChar x)
  decodeBlankSpaces

decodeChar :: Char -> ByteDecoder.ByteDecoder ()
decodeChar x = do
  y <- ByteDecoder.word8
  Monad.when (fromIntegral (fromEnum x) /= y)
    (fail ("expected " <> show x <> " but got " <> show y))

decodeBoolean :: ByteDecoder.ByteDecoder Json
decodeBoolean = decodeFalse Applicative.<|> decodeTrue

decodeFalse :: ByteDecoder.ByteDecoder Json
decodeFalse = do
  decodeSymbol "false"
  pure (Boolean False)

decodeTrue :: ByteDecoder.ByteDecoder Json
decodeTrue = do
  decodeSymbol "true"
  pure (Boolean True)

decodeNumber :: ByteDecoder.ByteDecoder Json
decodeNumber = do
  xs <- ByteDecoder.munch (\ x -> 0x30 <= x && x <= 0x39 || x == 0x2e)
  decodeBlankSpaces
  let ys = Text.unpack (Text.decodeUtf8 xs)
  if null ys
    then fail "empty"
    else pure (Number (read ys))

decodeString :: ByteDecoder.ByteDecoder Json
decodeString = do
  x <- decodeText
  pure (String x)

decodeText :: ByteDecoder.ByteDecoder Text.Text
decodeText = decodeBetween (decodeSymbol "\"") (decodeSymbol "\"")
  (fmap Text.decodeUtf8 (ByteDecoder.munch (/= 0x22)))

decodeArray :: ByteDecoder.ByteDecoder Json
decodeArray = do
  elements <- decodeBetween (decodeSymbol "[") (decodeSymbol "]")
    (decodeSeparated (decodeSymbol ",") decodeJson)
  pure (Array elements)

decodeObject :: ByteDecoder.ByteDecoder Json
decodeObject = do
  pairs <- decodeBetween (decodeSymbol "{") (decodeSymbol "}")
    (decodeSeparated (decodeSymbol ",") decodePair)
  pure (Object pairs)

decodeBetween :: ByteDecoder.ByteDecoder l -> ByteDecoder.ByteDecoder r -> ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder a
decodeBetween l r d = do
  Monad.void l
  x <- d
  Monad.void r
  pure x

decodeSeparated :: ByteDecoder.ByteDecoder s -> ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder (List.List a)
decodeSeparated s d = do
  x <- d
  xs <- decodeSome (do
    Monad.void s
    d)
  pure (List.Node x xs)

decodeSome :: ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder (List.List a)
decodeSome d = decodeMany d Applicative.<|> pure List.Empty

decodeMany :: ByteDecoder.ByteDecoder a -> ByteDecoder.ByteDecoder (List.List a)
decodeMany d = do
  x <- d
  xs <- decodeSome d
  pure (List.Node x xs)

decodePair :: ByteDecoder.ByteDecoder (Pair.Pair Text.Text Json)
decodePair = do
  k <- decodeText
  decodeSymbol ":"
  v <- decodeJson
  pure (Pair.Pair k v)

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

encodeArray :: Array -> Builder.Builder
encodeArray xs = case xs of
  List.Empty -> Builder.string7 "[]"
  List.Node x ys -> Builder.string7 "[ " <> encode x <> encodeArrayHelper ys

encodeArrayHelper :: Array -> Builder.Builder
encodeArrayHelper xs = case xs of
  List.Empty -> Builder.string7 " ]"
  List.Node x ys -> Builder.string7 ",\n" <> encode x <> encodeArrayHelper ys

encodeObject :: Object -> Builder.Builder
encodeObject xs = case xs of
  List.Empty -> Builder.string7 "{}"
  List.Node x ys -> Builder.string7 "{ " <> encodePair x <> encodeObjectHelper ys

encodePair :: Pair.Pair Text.Text Json -> Builder.Builder
encodePair (Pair.Pair k v) = encodeString k <> Builder.string7 ": " <> encode v

encodeObjectHelper :: Object -> Builder.Builder
encodeObjectHelper xs = case xs of
  List.Empty -> Builder.string7 " }"
  List.Node x ys -> encodePair x <> Builder.string7 ",\n" <> encodeObjectHelper ys