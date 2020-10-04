module RocketLeague.Version where

import qualified Data.ByteString.Builder as Builder
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteDecoder as ByteDecoder
import qualified Zippy.Type.Json as Json

data Version = Version
  { major :: U32.U32
  } deriving (Eq, Show)

decode :: ByteDecoder.ByteDecoder Version
decode = fail "Version/decode"

encode :: Version -> Builder.Builder
encode = error "Version/encode"

fromJson :: Json.Json -> Either String Version
fromJson _ = Left "Version/fromJson"

toJson :: Version -> Json.Json
toJson = error "Version/toJson"
