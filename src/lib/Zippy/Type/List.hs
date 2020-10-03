module Zippy.Type.List where

data List a
  = Empty
  | Node a (List a)
  deriving (Eq, Show)

fromList :: [a] -> List a
fromList = foldr Node Empty

toList :: List a -> [a]
toList xs = case xs of
  Empty -> []
  Node x ys -> x : toList ys
