module Zippy.JsonDecoder where

import qualified Data.Functor.Identity as Identity
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.Result as Result

type JsonDecoder = Decoder.Decoder Json.Json () Identity.Identity

run :: JsonDecoder a -> Json.Json -> Result.Result String a
run = Decoder.runSimple
