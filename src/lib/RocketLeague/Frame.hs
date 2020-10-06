module RocketLeague.Frame where

import qualified RocketLeague.F32 as F32
import qualified Zippy.Class.FromBits as FromBits
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBits as ToBits
import qualified Zippy.Class.ToJson as ToJson

data Frame = Frame
  { time :: F32.F32
  , delta :: F32.F32
  } deriving (Eq, Show)

instance FromBits.FromBits Frame where
  fromBits = error "Frame/fromBits"

instance FromJson.FromJson Frame where
  fromJson = error "Frame/fromJson"

instance ToBits.ToBits Frame where
  toBits = error "Frame/toBits"

instance ToJson.ToJson Frame where
  toJson = error "Frame/toJson"
