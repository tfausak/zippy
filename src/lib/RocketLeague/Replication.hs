module RocketLeague.Replication (Replication(..), fromBits) where

import qualified RocketLeague.CompressedWord as CompressedWord
import qualified RocketLeague.Replication.Destroyed as DestroyedReplication
import qualified RocketLeague.Replication.Spawned as SpawnedReplication
import qualified RocketLeague.Replication.Updated as UpdatedReplication
import qualified RocketLeague.Context as Context
import qualified Zippy.BitGet as BitGet

data Replication
  = Spawned SpawnedReplication.SpawnedReplication
  | Updated UpdatedReplication.UpdatedReplication
  | Destroyed DestroyedReplication.DestroyedReplication
  deriving (Eq, Show)

fromBits :: Context.Context -> BitGet.BitGet Replication
fromBits context = BitGet.label "Replication" $ do
  actorId <- BitGet.label "actorId" $ CompressedWord.fromBits context
  isOpen <- BitGet.bool
  if isOpen
    then do
      isNew <- BitGet.bool
      if isNew
        then Spawned <$> SpawnedReplication.fromBits context actorId
        else Updated <$> UpdatedReplication.fromBits actorId
    else Destroyed <$> DestroyedReplication.fromBits actorId
