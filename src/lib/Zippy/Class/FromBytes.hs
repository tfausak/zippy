module Zippy.Class.FromBytes where

import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Zippy.Convert as Convert
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type ByteDecoder = Decoder.Decoder ByteString.ByteString () Identity.Identity

class FromBytes a where
  fromBytes :: ByteDecoder a

instance FromBytes Bool where
  fromBytes = fmap (/= (0x00 :: Word.Word8)) fromBytes

instance FromBytes Float where
  fromBytes = fmap Convert.unsafeWord32ToFloat fromBytes

instance FromBytes Int.Int8 where
  fromBytes = fmap Convert.word8ToInt8 fromBytes

instance FromBytes Int.Int16 where
  fromBytes = fmap Convert.word16ToInt16 fromBytes

instance FromBytes Int.Int32 where
  fromBytes = fmap Convert.word32ToInt32 fromBytes

instance FromBytes Json.Json where
  fromBytes = Json.decode

instance (FromBytes a, FromBytes b) => FromBytes (Pair.Pair a b) where
  fromBytes = do
    x <- fromBytes
    y <- fromBytes
    pure $ Pair.Pair x y

instance FromBytes Word.Word8 where
  fromBytes = fmap ByteString.head $ count 1

instance FromBytes Word.Word16 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word8ToWord16 8 lo hi

instance FromBytes Word.Word32 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word16ToWord32 16 lo hi

instance FromBytes Word.Word64 where
  fromBytes = do
    lo <- fromBytes
    hi <- fromBytes
    pure $ Convert.combine Convert.word32ToWord64 32 lo hi

count :: Int -> ByteDecoder ByteString.ByteString
count n = Decoder.Decoder $ \ s1 _ ->
  let size = ByteString.length s1 in if n > size then
    pure
      . Result.Fail
      . Pair.Pair List.Empty
      $ "not enough input (wanted "
      <> show n
      <> " but have "
      <> show size
      <> ")"
  else let (x, s2) = ByteString.splitAt n s1 in
    pure . Result.Pass $ Pair.Pair s2 x

munch :: (Word.Word8 -> Bool) -> ByteDecoder ByteString.ByteString
munch f = Decoder.Decoder $ \ s1 _ ->
  let (x, s2) = ByteString.span f s1
  in pure . Result.Pass $ Pair.Pair s2 x
