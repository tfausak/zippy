module RocketLeague.Version (Version(..), fromBytes) where

import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Version = Version
  { major :: U32.U32
  , minor :: U32.U32
  , patch :: Maybe U32.U32
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Version
fromBytes = ByteGet.label "Version" $ do
  major <- ByteGet.label "major" U32.fromBytes
  minor <- ByteGet.label "minor" U32.fromBytes
  patch <- ByteGet.label "patch" $
    if U32.toWord32 major >= 868 && U32.toWord32 minor >= 18
    then fmap Just U32.fromBytes
    else pure Nothing
  pure Version { major, minor, patch }
