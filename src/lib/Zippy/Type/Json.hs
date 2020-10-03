module Zippy.Type.Json where

import qualified Data.ByteString as ByteString

data Json
  = Null
  deriving (Eq, Show)

decode :: ByteString.ByteString -> Either String Json
decode _ = Right Null

encode :: Json -> ByteString.ByteString
encode _ = ByteString.empty
