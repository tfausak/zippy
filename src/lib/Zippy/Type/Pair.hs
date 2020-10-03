module Zippy.Type.Pair where

data Pair a b
  = Pair a b
  deriving (Eq, Show)

fromTuple :: (a, b) -> Pair a b
fromTuple = uncurry Pair

toTuple :: Pair a b -> (a, b)
toTuple (Pair x y) = (x, y)
