module Zippy.Class.ToJson where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

class ToJson a where
  toJson :: a -> Json.Json

instance ToJson Int.Int8 where
  toJson = Json.Number . Convert.int8ToDouble

instance ToJson Int.Int16 where
  toJson = Json.Number . Convert.int16ToDouble

instance ToJson Int.Int32 where
  toJson = Json.Number . Convert.int32ToDouble

instance ToJson a => ToJson (Option.Option a) where
  toJson = Option.option Json.Null toJson

instance ToJson Text.Text where
  toJson = Json.String

instance ToJson Word.Word8 where
  toJson = Json.Number . Convert.word8ToDouble

instance ToJson Word.Word16 where
  toJson = Json.Number . Convert.word16ToDouble

instance ToJson Word.Word32 where
  toJson = Json.Number . Convert.word32ToDouble

object :: [(String, Json.Json)] -> Json.Json
object = Json.Object . List.fromList . fmap (Pair.fromTuple . Bifunctor.first Text.pack)
