module Zippy.ByteDecoder where

import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.Word as Word
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type ByteDecoder = Decoder.Decoder ByteString.ByteString () Identity.Identity

run :: ByteDecoder a -> ByteString.ByteString -> Either String a
run d s = case Identity.runIdentity (Decoder.run d s ()) of
  Result.Fail p -> Left (show p)
  Result.Pass (Pair.Pair _ x) -> Right x

count :: Int -> ByteDecoder ByteString.ByteString
count n = Decoder.Decoder $ \ s1 _ -> if n > ByteString.length s1
  then pure (Result.Fail (Pair.Pair List.Empty "not enough input"))
  else let (x, s2) = ByteString.splitAt n s1
    in pure (Result.Pass (Pair.Pair s2 x))

label :: String -> ByteDecoder a -> ByteDecoder a
label = Decoder.label

word8 :: ByteDecoder Word.Word8
word8 = fmap ByteString.head (count 1)
