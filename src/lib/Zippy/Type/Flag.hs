module Zippy.Type.Flag where

data Flag
  = Help
  | Input FilePath
  | Mode String
  | Output FilePath
  | Version
  deriving (Eq, Show)
