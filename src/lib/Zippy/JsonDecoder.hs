module Zippy.JsonDecoder where

import qualified Data.Functor.Identity as Identity
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json

type JsonDecoder = Decoder.Decoder Json.Json () Identity.Identity
