module Zippy.Class.FromBits where

import qualified Data.Functor.Identity as Identity
import qualified Zippy.Type.BitString as BitString
import qualified Zippy.Type.Decoder as Decoder

type BitDecoder = Decoder.Decoder BitString.BitString () Identity.Identity

class FromBits a where
  fromBits :: BitDecoder a
