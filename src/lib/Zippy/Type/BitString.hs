module Zippy.Type.BitString where

import qualified Data.ByteString as ByteString

data BitString = BitString
  { byteString :: ByteString.ByteString
  , offset :: Int
  } deriving (Eq, Show)
