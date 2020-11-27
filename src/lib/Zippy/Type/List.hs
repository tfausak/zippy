module Zippy.Type.List where

import qualified Zippy.Type.Option as Option
import qualified Zippy.Type.Pair as Pair

data List a
  = Empty
  | Node a (List a)
  deriving (Eq, Show)

instance Foldable List where
  foldr f z xs = case xs of
    Empty -> z
    Node x ys -> foldr f (f x z) ys

instance Functor List where
  fmap f xs = case xs of
    Empty -> Empty
    Node x ys -> Node (f x) $ fmap f ys

instance Traversable List where
  traverse f = fmap fromList . traverse f . toList

find :: Eq k => k -> List (Pair.Pair k v) -> Option.Option v
find k xs = case xs of
  Empty -> Option.None
  Node (Pair.Pair j v) ys -> if k == j then Option.Some v else find k ys

fromList :: [a] -> List a
fromList = foldr Node Empty

reverse :: List a -> List a
reverse = reverseWith Empty

reverseWith :: List a -> List a -> List a
reverseWith list xs = case xs of
  Empty -> list
  Node x ys -> reverseWith (Node x list) ys

toList :: List a -> [a]
toList xs = case xs of
  Empty -> []
  Node x ys -> x : toList ys
