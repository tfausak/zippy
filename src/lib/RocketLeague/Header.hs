module RocketLeague.Header (Header(..), fromBytes) where

import qualified RocketLeague.Dict as Dict
import qualified RocketLeague.Property as Property
import qualified RocketLeague.Str as Str
import qualified RocketLeague.Version as Version
import qualified Zippy.ByteGet as ByteGet

data Header = Header
  { version :: Version.Version
  , label :: Str.Str
  , properties :: Dict.Dict Property.Property
  } deriving (Eq, Show)

fromBytes :: ByteGet.ByteGet Header
fromBytes = ByteGet.label "Header" $ do
  version <- ByteGet.label "version" Version.fromBytes
  label <- ByteGet.label "label" Str.fromBytes
  properties <- ByteGet.label "properties" $ Dict.fromBytes Property.fromBytes
  pure Header { version, label, properties }
