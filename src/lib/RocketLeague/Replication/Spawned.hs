module RocketLeague.Replication.Spawned (SpawnedReplication(..), fromBits) where

import qualified RocketLeague.CompressedWord as CompressedWord
import qualified RocketLeague.Context as Context
import qualified Zippy.BitGet as BitGet

newtype SpawnedReplication = SpawnedReplication
  { actorId :: CompressedWord.CompressedWord
  -- TODO: Add more fields.
  } deriving (Eq, Show)

fromBits :: Context.Context -> CompressedWord.CompressedWord -> BitGet.BitGet SpawnedReplication
fromBits _context _actorId = BitGet.label "SpawnedReplication" $ do
  fail "TODO"
