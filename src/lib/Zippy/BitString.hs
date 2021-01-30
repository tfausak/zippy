module Zippy.BitString (BitString(..), fromByteString) where

import qualified Data.ByteString as ByteString

data BitString = BitString
  { byteString :: ByteString.ByteString
  , index :: Int
  } deriving (Eq, Show)

fromByteString :: ByteString.ByteString -> BitString
fromByteString byteString = BitString { byteString, index = 0 }
