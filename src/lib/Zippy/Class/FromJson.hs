module Zippy.Class.FromJson where

import qualified Control.Monad.Fail as Fail
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

type JsonDecoder = Decoder.Decoder Json.Json () Identity.Identity

class FromJson a where
  fromJson :: JsonDecoder a

instance FromJson Json.Json where
  fromJson = Decoder.get

instance FromJson a => FromJson [a] where
  fromJson = fmap List.toList fromJson

instance FromJson Bool where
  fromJson = boolean pure

instance FromJson Float where
  fromJson = number $ pure . Convert.doubleToFloat

instance FromJson Int.Int8 where
  fromJson = number Convert.doubleToInt8

instance FromJson Int.Int16 where
  fromJson = number Convert.doubleToInt16

instance FromJson Int.Int32 where
  fromJson = number Convert.doubleToInt32

instance FromJson a => FromJson (List.List a) where
  fromJson = array . traverse $ \ j -> Decoder.embed fromJson j ()

instance FromJson a => FromJson (Option.Option a) where
  fromJson = do
    j <- Decoder.get
    case j of
      Json.Null -> pure Option.None
      _ -> fmap Option.Some $ Decoder.embed fromJson j ()

instance (FromJson a, FromJson b) => FromJson (Pair.Pair a b) where
  fromJson = do
    [jx, jy] <- fromJson
    x <- Decoder.embed fromJson jx ()
    y <- Decoder.embed fromJson jy ()
    pure $ Pair.Pair x y

instance FromJson Text.Text where
  fromJson = string pure

instance FromJson Word.Word8 where
  fromJson = number Convert.doubleToWord8

instance FromJson Word.Word16 where
  fromJson = number Convert.doubleToWord16

instance FromJson Word.Word32 where
  fromJson = number Convert.doubleToWord32

instance FromJson Word.Word64 where
  fromJson = string $ either Fail.fail pure . Read.readEither . Text.unpack

array :: (Json.Array -> JsonDecoder a) -> JsonDecoder a
array f = do
  j <- Decoder.get
  case j of
    Json.Array x -> f x
    _ -> Fail.fail $ "expected array but got " <> show j

boolean :: (Bool -> JsonDecoder a) -> JsonDecoder a
boolean f = do
  j <- Decoder.get
  case j of
    Json.Boolean x -> f x
    _ -> Fail.fail $ "expected boolean but got " <> show j

number :: (Double -> JsonDecoder a) -> JsonDecoder a
number f = do
  j <- Decoder.get
  case j of
    Json.Number x -> f x
    _ -> Fail.fail $ "expected number but got " <> show j

object :: (Json.Object -> JsonDecoder a) -> JsonDecoder a
object f = do
  j <- Decoder.get
  case j of
    Json.Object x -> f x
    _ -> Fail.fail $ "expected object but got " <> show j

optional :: FromJson a => Json.Object -> String -> JsonDecoder (Option.Option a)
optional o k = case List.find (Text.pack k) o of
  Option.None -> pure Option.None
  Option.Some j -> Decoder.embed fromJson j ()

required :: FromJson a => Json.Object -> String -> JsonDecoder a
required o k = case List.find (Text.pack k) o of
  Option.None -> Fail.fail $ "missing required key " <> show k
  Option.Some j -> Decoder.embed fromJson j ()

string :: (Text.Text -> JsonDecoder a) -> JsonDecoder a
string f = do
  j <- Decoder.get
  case j of
    Json.String x -> f x
    _ -> Fail.fail $ "expected string but got " <> show j
