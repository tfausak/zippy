module Zippy.Class.ToJson where

import qualified Data.Word as Word
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.Option as Option

class ToJson a where
  toJson :: a -> Json.Json

instance ToJson a => ToJson (Option.Option a) where
  toJson = Option.option Json.Null toJson

instance ToJson Word.Word8 where
  toJson = Json.Number . Convert.word8ToDouble

instance ToJson Word.Word16 where
  toJson = Json.Number . Convert.word16ToDouble

instance ToJson Word.Word32 where
  toJson = Json.Number . Convert.word32ToDouble
