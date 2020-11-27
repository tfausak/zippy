module Zippy.Type.Pair where

import qualified Data.Bifunctor as Bifunctor

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Bifunctor.Bifunctor Pair where
  bimap f g (Pair x y) = Pair (f x) (g y)

fromTuple :: (a, b) -> Pair a b
fromTuple = uncurry Pair

toTuple :: Pair a b -> (a, b)
toTuple (Pair x y) = (x, y)
