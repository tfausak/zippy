module Zippy.Type.Result where

data Result a b
  = Fail a
  | Pass b
  deriving (Eq, Show)

fromEither :: Either a b -> Result a b
fromEither = either Fail Pass

toEither :: Result a b -> Either a b
toEither x = case x of
  Fail y -> Left y
  Pass y -> Right y
