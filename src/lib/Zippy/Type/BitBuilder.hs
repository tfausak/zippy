module Zippy.Type.BitBuilder where

import qualified Data.ByteString.Builder as Builder

data BitBuilder = BitBuilder
  { builder :: Builder.Builder
  , offset :: Int
  }
