module Zippy.Class.FromJson where

import qualified Data.Word as Word
import qualified Zippy.Convert as Convert
import qualified Zippy.JsonDecoder as JsonDecoder

class FromJson a where
  fromJson :: JsonDecoder.JsonDecoder a

instance FromJson Word.Word8 where
  fromJson = JsonDecoder.number Convert.doubleToWord8

instance FromJson Word.Word16 where
  fromJson = JsonDecoder.number Convert.doubleToWord16

instance FromJson Word.Word32 where
  fromJson = JsonDecoder.number Convert.doubleToWord32
