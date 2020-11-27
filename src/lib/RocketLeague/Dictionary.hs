module RocketLeague.Dictionary where

import qualified Data.Bifunctor as Bifunctor
import qualified RocketLeague.Str as Str
import qualified Zippy.Class.FromBytes as FromBytes
import qualified Zippy.Class.FromJson as FromJson
import qualified Zippy.Class.ToBytes as ToBytes
import qualified Zippy.Class.ToJson as ToJson
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair

newtype Dictionary a = Dictionary
  { value :: List.List (Pair.Pair Str.Str a)
  } deriving (Eq, Show)

instance FromBytes.FromBytes a => FromBytes.FromBytes (Dictionary a) where
  fromBytes = fromBytesWith List.Empty

instance FromJson.FromJson a => FromJson.FromJson (Dictionary a) where
  fromJson = fmap Dictionary
    . FromJson.object
    . traverse
    $ \ (Pair.Pair k v) -> fmap (Pair.Pair $ Str.Str k)
      $ Decoder.embed FromJson.fromJson v ()

instance ToBytes.ToBytes a => ToBytes.ToBytes (Dictionary a) where
  toBytes x =
    foldMap ToBytes.toBytes (value x)
    <> ToBytes.toBytes (Str.fromString "None")

instance ToJson.ToJson a => ToJson.ToJson (Dictionary a) where
  toJson = Json.Object . fmap (Bifunctor.bimap Str.value ToJson.toJson) . value

fromBytesWith
  :: FromBytes.FromBytes a
  => List.List (Pair.Pair Str.Str a)
  -> FromBytes.ByteDecoder (Dictionary a)
fromBytesWith list = do
  key <- FromBytes.fromBytes
  if key == Str.fromString "None"
    then pure $ Dictionary list
    else do
      value <- FromBytes.fromBytes
      fromBytesWith $ List.Node (Pair.Pair key value) list
