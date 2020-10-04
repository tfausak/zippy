module Zippy.Type.Result where

data Result a b
  = Fail a
  | Pass b
  deriving (Eq, Show)

fromEither :: Either a b -> Result a b
fromEither = either Fail Pass

result :: (a -> c) -> (b -> c) -> Result a b -> c
result f g x = case x of
  Fail y -> f y
  Pass y -> g y

toEither :: Result a b -> Either a b
toEither = result Left Right
