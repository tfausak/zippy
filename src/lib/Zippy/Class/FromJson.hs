module Zippy.Class.FromJson where

import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Zippy.Convert as Convert
import qualified Zippy.JsonDecoder as JsonDecoder
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option

class FromJson a where
  fromJson :: JsonDecoder.JsonDecoder a

instance FromJson Int.Int8 where
  fromJson = number Convert.doubleToInt8

instance FromJson Int.Int16 where
  fromJson = number Convert.doubleToInt16

instance FromJson Int.Int32 where
  fromJson = number Convert.doubleToInt32

instance FromJson a => FromJson (Option.Option a) where
  fromJson = do
    j <- Decoder.get
    case j of
      Json.Null -> pure Option.None
      _ -> fmap Option.Some $ Decoder.embed fromJson j ()

instance FromJson Text.Text where
  fromJson = string pure

instance FromJson Word.Word8 where
  fromJson = number Convert.doubleToWord8

instance FromJson Word.Word16 where
  fromJson = number Convert.doubleToWord16

instance FromJson Word.Word32 where
  fromJson = number Convert.doubleToWord32

number :: (Double -> JsonDecoder.JsonDecoder a) -> JsonDecoder.JsonDecoder a
number f = do
  j <- Decoder.get
  case j of
    Json.Number x -> f x
    _ -> fail $ "expected number but got " <> show j

object :: (Json.Object -> JsonDecoder.JsonDecoder a) -> JsonDecoder.JsonDecoder a
object f = do
  j <- Decoder.get
  case j of
    Json.Object x -> f x
    _ -> fail $ "expected object but got " <> show j

optional :: FromJson a => Json.Object -> String -> JsonDecoder.JsonDecoder (Option.Option a)
optional o k = case List.find (Text.pack k) o of
  Option.None -> pure Option.None
  Option.Some j -> Decoder.embed fromJson j ()

required :: FromJson a => Json.Object -> String -> JsonDecoder.JsonDecoder a
required o k = case List.find (Text.pack k) o of
  Option.None -> fail $ "missing required key " <> show k
  Option.Some j -> Decoder.embed fromJson j ()

string :: (Text.Text -> JsonDecoder.JsonDecoder a) -> JsonDecoder.JsonDecoder a
string f = do
  j <- Decoder.get
  case j of
    Json.String x -> f x
    _ -> fail $ "expected string but got " <> show j
