module Zippy.ByteDecoder where

import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Word as Word
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type ByteDecoder = Decoder.Decoder ByteString.ByteString () Identity.Identity

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

word8 :: ByteDecoder Word.Word8
word8 = fmap ByteString.head $ count 1
