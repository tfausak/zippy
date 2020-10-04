module RocketLeague.U16 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified RocketLeague.U8 as U8
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json

newtype U16 = U16
  { value :: Word.Word16
  } deriving (Eq, Show)

instance FromJson.FromJson U16 where
  fromJson = fmap U16 FromJson.fromJson

decode :: ByteDecoder.ByteDecoder U16
decode = ByteDecoder.label "U16" $ do
  lo <- U8.decode
  hi <- U8.decode
  pure . U16 $ Convert.combine (Convert.word8ToWord16 . U8.value) 8 lo hi

encode :: U16 -> Builder.Builder
encode = Builder.word16LE . value

toJson :: U16 -> Json.Json
toJson = Json.Number . Convert.word16ToDouble . value
