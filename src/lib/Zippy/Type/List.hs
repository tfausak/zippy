module Zippy.Type.List where

import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

data List a
  = Empty
  | Node a (List a)
  deriving (Eq, Show)

find :: Eq k => k -> List (Pair.Pair k v) -> Option.Option v
find k xs = case xs of
  Empty -> Option.None
  Node (Pair.Pair j v) ys -> if k == j then Option.Some v else find k ys

fromList :: [a] -> List a
fromList = foldr Node Empty

toList :: List a -> [a]
toList xs = case xs of
  Empty -> []
  Node x ys -> x : toList ys
