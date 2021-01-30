module RocketLeague.Property.Array (ArrayProperty, fromVec, toVec, fromBytes) where

import qualified RocketLeague.Dict as Dict
import qualified RocketLeague.Vec as Vec
import qualified Zippy.ByteGet as ByteGet

newtype ArrayProperty property
  = ArrayProperty (Vec.Vec (Dict.Dict property))
  deriving (Eq, Show)

fromVec :: Vec.Vec (Dict.Dict property) -> ArrayProperty property
fromVec = ArrayProperty

toVec :: ArrayProperty property -> Vec.Vec (Dict.Dict property)
toVec (ArrayProperty x) = x

fromBytes :: ByteGet.ByteGet property -> ByteGet.ByteGet (ArrayProperty property)
fromBytes getItem = ByteGet.label "ArrayProperty" . fmap fromVec . Vec.fromBytes $ Dict.fromBytes getItem
