module RocketLeague.Replication.Updated (UpdatedReplication(..), fromBits) where

import qualified Zippy.BitGet as BitGet
import qualified RocketLeague.CompressedWord as CompressedWord

data UpdatedReplication = UpdatedReplication
  { actorId :: CompressedWord.CompressedWord
  } deriving (Eq, Show)

fromBits :: CompressedWord.CompressedWord -> BitGet.BitGet UpdatedReplication
fromBits actorId = BitGet.label "UpdatedReplication" $ do
  fail "TODO"
  pure UpdatedReplication { actorId }
