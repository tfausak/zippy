module Zippy.Class.ToBits where

import qualified Zippy.Type.BitBuilder as BitBuilder

class ToBits a where
  toBits :: a -> BitBuilder.BitBuilder
