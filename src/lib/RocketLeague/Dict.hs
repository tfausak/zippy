module RocketLeague.Dict (Dict, fromArray, toArray, fromBytes, RocketLeague.Dict.lookup) where

import qualified Data.Array as Array
import qualified RocketLeague.Str as Str
import qualified Zippy.ByteGet as ByteGet

newtype Dict a
  = Dict (Array.Array Int (Str.Str, a))
  deriving (Eq, Show)

fromArray :: Array.Array Int (Str.Str, a) -> Dict a
fromArray = Dict

toArray :: Dict a -> Array.Array Int (Str.Str, a)
toArray (Dict x) = x

fromBytes :: ByteGet.ByteGet a -> ByteGet.ByteGet (Dict a)
fromBytes getItem = ByteGet.label "Dict" $ fromBytesWith getItem 0 []

fromBytesWith
  :: ByteGet.ByteGet a
  -> Int
  -> [(Int, (Str.Str, a))]
  -> ByteGet.ByteGet (Dict a)
fromBytesWith getItem index items = do
  key <- ByteGet.label "key" Str.fromBytes
  let none = Str.fromString "None"
  if key == none
    then pure . fromArray $ Array.array (0, index - 1) items
    else do
      value <- ByteGet.label (show $ Str.toString key) getItem
      fromBytesWith getItem (index + 1) $ (index, (key, value)) : items

lookup :: String -> Dict a -> Maybe a
lookup k = Prelude.lookup (Str.fromString k) . Array.elems . toArray
