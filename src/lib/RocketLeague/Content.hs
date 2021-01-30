module RocketLeague.Content (Content(..), fromBytes) where

import qualified RocketLeague.Cache as Cache
import qualified RocketLeague.ClassMapping as ClassMapping
import qualified RocketLeague.Keyframe as Keyframe
import qualified RocketLeague.Mark as Mark
import qualified RocketLeague.Message as Message
import qualified RocketLeague.Str as Str
import qualified RocketLeague.Vec as Vec
import qualified Zippy.ByteGet as ByteGet

data Content frames = Content
  { levels :: Vec.Vec Str.Str
  , keyframes :: Vec.Vec Keyframe.Keyframe
  , frames :: frames
  , messages :: Vec.Vec Message.Message
  , marks :: Vec.Vec Mark.Mark
  , packages :: Vec.Vec Str.Str
  , objects :: Vec.Vec Str.Str
  , names :: Vec.Vec Str.Str
  , classMappings :: Vec.Vec ClassMapping.ClassMapping
  , caches :: Vec.Vec Cache.Cache
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet frames -> ByteGet.ByteGet (Content frames)
fromBytes getFrames = ByteGet.label "Content" $ do
  levels <- ByteGet.label "levels" $ Vec.fromBytes Str.fromBytes
  keyframes <- ByteGet.label "keyframes" $ Vec.fromBytes Keyframe.fromBytes
  frames <- ByteGet.label "frames" getFrames
  messages <- ByteGet.label "messages" $ Vec.fromBytes Message.fromBytes
  marks <- ByteGet.label "marks" $ Vec.fromBytes Mark.fromBytes
  packages <- ByteGet.label "packages" $ Vec.fromBytes Str.fromBytes
  objects <- ByteGet.label "objects" $ Vec.fromBytes Str.fromBytes
  names <- ByteGet.label "names" $ Vec.fromBytes Str.fromBytes
  classMappings <- ByteGet.label "classMappings" $ Vec.fromBytes ClassMapping.fromBytes
  caches <- ByteGet.label "caches" $ Vec.fromBytes Cache.fromBytes
  pure Content { levels, keyframes, frames, messages, marks, packages, objects, names, classMappings, caches }
