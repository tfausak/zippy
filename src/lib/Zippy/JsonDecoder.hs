module Zippy.JsonDecoder where

import qualified Data.Functor.Identity as Identity
import qualified Data.Text as Text
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type JsonDecoder = Decoder.Decoder Json.Json () Identity.Identity

run :: JsonDecoder a -> Json.Json -> Result.Result String a
run = Decoder.runSimple

label :: String -> JsonDecoder a -> JsonDecoder a
label = Decoder.label

number :: (Double -> JsonDecoder a) -> JsonDecoder a
number f = Decoder.Decoder $ \ s u -> case s of
  Json.Number x -> Decoder.run (f x) s u
  _ -> pure . Result.Fail . Pair.Pair List.Empty $
    "expected number, got " <> show s

object :: (Json.Object -> JsonDecoder a) -> JsonDecoder a
object f = Decoder.Decoder $ \ s u -> case s of
  Json.Object x -> Decoder.run (f x) s u
  _ -> pure . Result.Fail . Pair.Pair List.Empty $
    "expected object, got " <> show s

required :: Json.Object -> String -> JsonDecoder a -> JsonDecoder a
required o k d = case List.find (Text.pack k) o of
  Option.None -> fail $ "missing required key: " <> show k
  Option.Some x -> Result.result fail pure $ run d x
