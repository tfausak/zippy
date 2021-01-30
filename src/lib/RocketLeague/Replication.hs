module RocketLeague.Replication (Replication(..), fromBits) where

import qualified RocketLeague.CompressedWord as CompressedWord
import qualified RocketLeague.Replication.Destroyed as DestroyedReplication
import qualified RocketLeague.Replication.Spawned as SpawnedReplication
import qualified RocketLeague.Replication.Updated as UpdatedReplication
import qualified Zippy.BitGet as BitGet

data Replication
  = Spawned SpawnedReplication.SpawnedReplication
  | Updated UpdatedReplication.UpdatedReplication
  | Destroyed DestroyedReplication.DestroyedReplication
  deriving (Eq, Show)

fromBits :: Int -> BitGet.BitGet Replication
fromBits maxChannels = BitGet.label "Replication" $ do
  actorId <- BitGet.label "actorId" $ CompressedWord.fromBits maxChannels
  isOpen <- BitGet.bool
  if isOpen
    then do
      isNew <- BitGet.bool
      if isNew
        then Spawned <$> SpawnedReplication.fromBits actorId
        else Updated <$> UpdatedReplication.fromBits actorId
    else Destroyed <$> DestroyedReplication.fromBits actorId
