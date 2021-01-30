module RocketLeague.Replication.Destroyed (DestroyedReplication(..), fromBits) where

import qualified Zippy.BitGet as BitGet
import qualified RocketLeague.CompressedWord as CompressedWord

newtype DestroyedReplication = DestroyedReplication
  { actorId :: CompressedWord.CompressedWord
  } deriving (Eq, Show)

fromBits :: CompressedWord.CompressedWord -> BitGet.BitGet DestroyedReplication
fromBits actorId = BitGet.label "DestroyedReplication" $ do
  pure DestroyedReplication { actorId }
