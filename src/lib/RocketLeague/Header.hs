module RocketLeague.Header where

import qualified RocketLeague.Dictionary as Dictionary
import qualified RocketLeague.Property as Property
import qualified RocketLeague.Str as Str
import qualified RocketLeague.Version as Version
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder

data Header = Header
  { version :: Version.Version
  , label :: Str.Str
  , properties :: Dictionary.Dictionary Property.Property
  } deriving (Eq, Show)

instance FromBytes.FromBytes Header where
  fromBytes = Decoder.label "Header" $ do
    version <- Decoder.label "version" FromBytes.fromBytes
    label <- Decoder.label "label" FromBytes.fromBytes
    properties <- Decoder.label "properties" FromBytes.fromBytes
    pure Header { version, label, properties }

instance FromJson.FromJson Header where
  fromJson = FromJson.object $ \ object -> do
    version <- FromJson.required object "version"
    label <- FromJson.required object "label"
    properties <- FromJson.required object "properties"
    pure Header { version, label, properties }

instance ToBytes.ToBytes Header where
  toBytes header = ToBytes.toBytes (version header)
    <> ToBytes.toBytes (label header)
    <> ToBytes.toBytes (properties header)

instance ToJson.ToJson Header where
  toJson header = ToJson.object
    [ ("version", ToJson.toJson $ version header)
    , ("label", ToJson.toJson $ label header)
    , ("properties", ToJson.toJson $ properties header)
    ]
