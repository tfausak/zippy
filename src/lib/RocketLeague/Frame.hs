module RocketLeague.Frame (Frame(..), fromBits) where

import qualified RocketLeague.F32 as F32
import qualified RocketLeague.Replication as Replication
import qualified RocketLeague.Vec as Vec
import qualified Zippy.BitGet as BitGet

data Frame = Frame
  { time :: F32.F32
  , delta :: F32.F32
  , replications :: Vec.Vec Replication.Replication
  } deriving (Eq, Show)

fromBits :: Int -> BitGet.BitGet Frame
fromBits maxChannels = BitGet.label "Frame" $ do
  time <- BitGet.label "time" F32.fromBits
  delta <- BitGet.label "delta" F32.fromBits
  replications <- BitGet.label "replications" . Vec.fromBits $ Replication.fromBits maxChannels
  pure Frame { time, delta, replications }
