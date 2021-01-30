module RocketLeague.Replication.Spawned (SpawnedReplication(..), fromBits) where

import qualified RocketLeague.CompressedWord as CompressedWord
import qualified Zippy.BitGet as BitGet

data SpawnedReplication = SpawnedReplication
  { actorId :: CompressedWord.CompressedWord
  } deriving (Eq, Show)

fromBits :: CompressedWord.CompressedWord -> BitGet.BitGet SpawnedReplication
fromBits actorId = BitGet.label "SpawnedReplication" $ do
  fail "TODO"
  pure SpawnedReplication { actorId }
