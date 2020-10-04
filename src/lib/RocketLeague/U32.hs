module RocketLeague.U32 where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word
import qualified RocketLeague.U16 as U16
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Json as Json

newtype U32 = U32
  { value :: Word.Word32
  } deriving (Eq, Show)

instance FromJson.FromJson U32 where
  fromJson = fmap U32 FromJson.fromJson

decode :: ByteDecoder.ByteDecoder U32
decode = ByteDecoder.label "U32" $ do
  lo <- U16.decode
  hi <- U16.decode
  pure . U32 $ Convert.combine (Convert.word16ToWord32 . U16.value) 16 lo hi

encode :: U32 -> Builder.Builder
encode = Builder.word32LE . value

toJson :: U32 -> Json.Json
toJson = Json.Number . Convert.word32ToDouble . value
