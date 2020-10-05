module RocketLeague.Str where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified RocketLeague.I32 as I32
import qualified Zippy.Convert as Convert
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson

newtype Str = Str
  { value :: Text.Text
  } deriving (Eq, Show)

instance FromBytes.FromBytes Str where
  fromBytes = do
    size <- fmap (Convert.int32ToInt . I32.value) FromBytes.fromBytes
    fmap (Str . Text.filter (not . isNull)) $ if size < 0
      then fromUtf16Bytes size
      else fromLatin1Bytes size

instance FromJson.FromJson Str where
  fromJson = fmap Str FromJson.fromJson

instance ToBytes.ToBytes Str where
  toBytes =
    (\ t -> if isLatin1 t then toLatin1Bytes t else toUtf16Bytes t)
    . appendNull
    . value

instance ToJson.ToJson Str where
  toJson = ToJson.toJson . value

appendNull :: Text.Text -> Text.Text
appendNull t = if Text.null t then t else Text.snoc t '\x00'

fromLatin1Bytes :: Int -> FromBytes.ByteDecoder Text.Text
fromLatin1Bytes =
  fmap Text.decodeLatin1
  . FromBytes.count
  . (\ n -> if n == 0x05000000 then 8 else n)

fromString :: String -> Str
fromString = Str . Text.pack

fromUtf16Bytes :: Int -> FromBytes.ByteDecoder Text.Text
fromUtf16Bytes =
  fmap (Text.decodeUtf16LEWith Text.lenientDecode)
  . FromBytes.count
  . (* 2)
  . negate

isLatin1 :: Text.Text -> Bool
isLatin1 = Text.all Char.isLatin1

isNull :: Char -> Bool
isNull = (== '\x00')

toLatin1Bytes :: Text.Text -> Builder.Builder
toLatin1Bytes t =
  (ToBytes.toBytes . I32.I32 . Convert.intToInt32 $ Text.length t)
  <> Builder.string8 (Text.unpack t)

toString :: Str -> String
toString = Text.unpack . value

toUtf16Bytes :: Text.Text -> Builder.Builder
toUtf16Bytes t =
  (ToBytes.toBytes . I32.I32 . Convert.intToInt32 . negate $ Text.length t)
  <> Builder.byteString (Text.encodeUtf16LE t)
