module Zippy.Type.Option where

data Option a
  = None
  | Some a
  deriving (Eq, Show)

fromMaybe :: Maybe a -> Option a
fromMaybe = maybe None Some

option :: b -> (a -> b) -> Option a -> b
option z f x = case x of
  None -> z
  Some y -> f y

toMaybe :: Option a -> Maybe a
toMaybe = option Nothing Just
