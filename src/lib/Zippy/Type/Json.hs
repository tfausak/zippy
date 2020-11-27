module Zippy.Type.Json where

import qualified Data.Text as Text
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair

data Json
  = Null
  | Boolean Bool
  | Number Double
  | String Text.Text
  | Array Array
  | Object Object
  deriving (Eq, Show)

type Array = List.List Json

type Object = List.List (Pair.Pair Text.Text Json)
