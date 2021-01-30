module RocketLeague.Replication.Updated (UpdatedReplication(..), fromBits) where

import qualified Zippy.BitGet as BitGet
import qualified RocketLeague.CompressedWord as CompressedWord

newtype UpdatedReplication = UpdatedReplication
  { actorId :: CompressedWord.CompressedWord
  -- TODO: Add more fields.
  } deriving (Eq, Show)

fromBits :: CompressedWord.CompressedWord -> BitGet.BitGet UpdatedReplication
fromBits _actorId = BitGet.label "UpdatedReplication" $ do
  fail "TODO"
