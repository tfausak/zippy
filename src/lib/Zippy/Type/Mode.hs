module Zippy.Type.Mode where

import qualified Zippy.Type.Result as Result

data Mode
  = Decode
  | Encode
  deriving (Eq, Show)

fromString :: String -> Result.Result String Mode
fromString string = case string of
  "decode" -> Result.Pass Decode
  "encode" -> Result.Pass Encode
  _ -> Result.Fail $ "invalid mode: " <> show string
