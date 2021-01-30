module RocketLeague.Replication.Spawned (SpawnedReplication(..), fromBits) where

import qualified RocketLeague.CompressedWord as CompressedWord
import qualified Zippy.BitGet as BitGet

newtype SpawnedReplication = SpawnedReplication
  { actorId :: CompressedWord.CompressedWord
  -- TODO: Add more fields.
  } deriving (Eq, Show)

fromBits :: CompressedWord.CompressedWord -> BitGet.BitGet SpawnedReplication
fromBits _actorId = BitGet.label "SpawnedReplication" $ do
  fail "TODO"
