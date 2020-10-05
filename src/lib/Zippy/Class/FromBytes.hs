module Zippy.Class.FromBytes where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Text.Read as Read
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type ByteDecoder = Decoder.Decoder ByteString.ByteString () Identity.Identity

class FromBytes a where
  fromBytes :: ByteDecoder a

instance FromBytes Bool where
  fromBytes = fmap (/= (0x00 :: Word.Word8)) fromBytes

instance FromBytes Float where
  fromBytes = fmap Convert.unsafeWord32ToFloat fromBytes

instance FromBytes Int.Int8 where
  fromBytes = fmap Convert.word8ToInt8 fromBytes

instance FromBytes Int.Int16 where
  fromBytes = fmap Convert.word16ToInt16 fromBytes

instance FromBytes Int.Int32 where
  fromBytes = fmap Convert.word32ToInt32 fromBytes

instance FromBytes Json.Json where
  fromBytes = do
    decodeBlankSpaces
    decodeJson

instance (FromBytes a, FromBytes b) => FromBytes (Pair.Pair a b) where
  fromBytes = do
    x <- fromBytes
    y <- fromBytes
    pure $ Pair.Pair x y

instance FromBytes Word.Word8 where
  fromBytes = fmap ByteString.head $ count 1

instance FromBytes Word.Word16 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word8ToWord16 8 lo hi

instance FromBytes Word.Word32 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word16ToWord32 16 lo hi

instance FromBytes Word.Word64 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word32ToWord64 32 lo hi

count :: Int -> ByteDecoder ByteString.ByteString
count n = Decoder.Decoder $ \ s1 _ ->
  let size = ByteString.length s1 in if n > size then
    pure
      . Result.Fail
      . Pair.Pair List.Empty
      $ "not enough input (wanted "
      <> show n
      <> " but have "
      <> show size
      <> ")"
  else let (x, s2) = ByteString.splitAt n s1 in
    pure . Result.Pass $ Pair.Pair s2 x

munch :: (Word.Word8 -> Bool) -> ByteDecoder ByteString.ByteString
munch f = Decoder.Decoder $ \ s1 _ ->
  let (x, s2) = ByteString.span f s1
  in pure . Result.Pass $ Pair.Pair s2 x

-------------------------------------------------------------------------------

decodeBlankSpaces :: ByteDecoder ()
decodeBlankSpaces = Monad.void (munch isBlankSpace)

isBlankSpace :: Word.Word8 -> Bool
isBlankSpace byte = case byte of
  0x09 -> True -- '\t'
  0x0a -> True -- '\n'
  0x0d -> True -- '\r'
  0x20 -> True -- ' '
  _ -> False

decodeJson :: ByteDecoder Json.Json
decodeJson = decodeNull
  Applicative.<|> decodeBoolean
  Applicative.<|> decodeNumber
  Applicative.<|> decodeString
  Applicative.<|> decodeArray
  Applicative.<|> decodeObject

decodeNull :: ByteDecoder Json.Json
decodeNull = do
  decodeSymbol "null"
  pure Json.Null

decodeSymbol :: String -> ByteDecoder ()
decodeSymbol string = do
  Monad.void $ mapM_ decodeChar string
  decodeBlankSpaces

decodeChar :: Char -> ByteDecoder ()
decodeChar expected = do
  actual <- fromBytes
  let
    charToWord8 :: Char -> Word.Word8
    charToWord8 = fromIntegral . fromEnum
    word8ToChar :: Word.Word8 -> Char
    word8ToChar = toEnum . fromIntegral
  Monad.when (charToWord8 expected /= actual)
    . Fail.fail
    $ "expected "
    <> show expected
    <> " but got "
    <> show (word8ToChar actual)

decodeBoolean :: ByteDecoder Json.Json
decodeBoolean = decodeFalse Applicative.<|> decodeTrue

decodeFalse :: ByteDecoder Json.Json
decodeFalse = do
  decodeSymbol "false"
  pure $ Json.Boolean False

decodeTrue :: ByteDecoder Json.Json
decodeTrue = do
  decodeSymbol "true"
  pure $ Json.Boolean True

decodeNumber :: ByteDecoder Json.Json
decodeNumber = do
  bytes <- munch $ \ x -> 0x30 <= x && x <= 0x39 || x == 0x2e || x == 0x2d
  decodeBlankSpaces
  case Read.readMaybe . Text.unpack $ Text.decodeUtf8 bytes of
    Nothing -> Fail.fail $ "decodeNumber: " <> show bytes
    Just x -> pure $ Json.Number x

decodeString :: ByteDecoder Json.Json
decodeString = fmap Json.String decodeText

decodeText :: ByteDecoder Text.Text
decodeText = decodeBetween (decodeSymbol "\"") (decodeSymbol "\"")
  . fmap Text.decodeUtf8
  $ munch (/= 0x22)

decodeArray :: ByteDecoder Json.Json
decodeArray = fmap Json.Array
  . decodeBetween (decodeSymbol "[") (decodeSymbol "]")
  $ decodeSeparated (decodeSymbol ",") decodeJson

decodeObject :: ByteDecoder Json.Json
decodeObject = fmap Json.Object
    . decodeBetween (decodeSymbol "{") (decodeSymbol "}")
    $ decodeSeparated (decodeSymbol ",") decodePair

decodeBetween
  :: ByteDecoder l
  -> ByteDecoder r
  -> ByteDecoder a
  -> ByteDecoder a
decodeBetween l r d = do
  Monad.void l
  x <- d
  Monad.void r
  pure x

decodeSeparated
  :: ByteDecoder s
  -> ByteDecoder a
  -> ByteDecoder (List.List a)
decodeSeparated s d =
  let
    many = do
      x <- d
      xs <- decodeSome $ do
        Monad.void s
        d
      pure $ List.Node x xs
  in many Applicative.<|> pure List.Empty

decodeSome :: ByteDecoder a -> ByteDecoder (List.List a)
decodeSome d = decodeMany d Applicative.<|> pure List.Empty

decodeMany :: ByteDecoder a -> ByteDecoder (List.List a)
decodeMany d = do
  x <- d
  xs <- decodeSome d
  pure $ List.Node x xs

decodePair :: ByteDecoder (Pair.Pair Text.Text Json.Json)
decodePair = do
  k <- decodeText
  decodeSymbol ":"
  v <- decodeJson
  pure $ Pair.Pair k v
