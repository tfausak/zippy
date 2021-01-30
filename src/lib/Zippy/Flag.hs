module Zippy.Flag (Flag(..)) where

data Flag
  = Help
  | Input FilePath
  | Output FilePath
  | Version
  deriving (Eq, Show)
