module Zippy.JsonDecoder where

import qualified Data.Functor.Identity as Identity
import qualified Data.Text as Text
import qualified Zippy.Type.Decoder as Decoder
import qualified Zippy.Type.Json as Json
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

type JsonDecoder = Decoder.Decoder Json.Json () Identity.Identity

run :: JsonDecoder a -> Json.Json -> Either String a
run d s = case Identity.runIdentity (Decoder.run d s ()) of
  Result.Fail p -> Left (show p)
  Result.Pass (Pair.Pair _ x) -> Right x

label :: String -> JsonDecoder a -> JsonDecoder a
label = Decoder.label

number :: (Double -> JsonDecoder a) -> JsonDecoder a
number f = Decoder.Decoder (\ s u -> case s of
  Json.Number x -> Decoder.run (f x) s u
  _ -> pure (Result.Fail (Pair.Pair List.Empty ("expected number, got " <> show s))))

object :: (Json.Object -> JsonDecoder a) -> JsonDecoder a
object f = Decoder.Decoder (\ s u -> case s of
  Json.Object x -> Decoder.run (f x) s u
  _ -> pure (Result.Fail (Pair.Pair List.Empty ("expected object, got " <> show s))))

required :: String -> Json.Object -> JsonDecoder a -> JsonDecoder a
required k o f = case myLookup (Text.pack k) o of
  Nothing -> fail $ "missing required key: " <> show k
  Just x -> either fail pure $ run f x

myLookup :: Eq k => k -> List.List (Pair.Pair k v) -> Maybe v
myLookup k xs = case xs of
  List.Empty -> Nothing
  List.Node (Pair.Pair j v) ys -> if k == j then Just v else myLookup k ys
