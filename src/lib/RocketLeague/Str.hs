module RocketLeague.Str (Str, fromText, fromString, toText, toString, fromBytes) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified RocketLeague.I32 as I32
import qualified Zippy.ByteGet as ByteGet

newtype Str
  = Str Text.Text
  deriving (Eq, Show)

fromText :: Text.Text -> Str
fromText = Str

fromString :: String -> Str
fromString = fromText . Text.pack

toText :: Str -> Text.Text
toText (Str x) = x

toString :: Str -> String
toString = Text.unpack . toText

fromBytes :: ByteGet.ByteGet Str
fromBytes = ByteGet.label "Str" $ do
  size <- ByteGet.label "size" $ fmap I32.toInt I32.fromBytes
  fmap (fromText . Text.dropEnd 1) $ if size < 0
    then fmap Text.decodeUtf16LE . ByteGet.take $ -2 * size
    else Text.decodeLatin1 <$> ByteGet.take size
