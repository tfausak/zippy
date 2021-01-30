module RocketLeague.Keyframe (Keyframe(..), fromBytes) where

import qualified RocketLeague.F32 as F32
import qualified RocketLeague.U32 as U32
import qualified Zippy.ByteGet as ByteGet

data Keyframe = Keyframe
  { time :: F32.F32
  , frame :: U32.U32
  , position :: U32.U32
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Keyframe
fromBytes = ByteGet.label "Keyframe" $ do
  time <- ByteGet.label "time" F32.fromBytes
  frame <- ByteGet.label "frame" U32.fromBytes
  position <- ByteGet.label "position" U32.fromBytes
  pure Keyframe { time, frame, position }
