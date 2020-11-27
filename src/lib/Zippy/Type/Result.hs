module Zippy.Type.Result where

data Result a b
  = Fail a
  | Pass b
  deriving (Eq, Show)

instance Functor (Result a) where
  fmap f = result Fail $ Pass . f

instance Applicative (Result a) where
  pure = Pass

  rf <*> rx = case (rf, rx) of
    (Fail e, _) -> Fail e
    (_, Fail e) -> Fail e
    (Pass f, Pass x) -> Pass $ f x

instance Monad (Result a) where
  r >>= f = result Fail f r

fromEither :: Either a b -> Result a b
fromEither = either Fail Pass

result :: (a -> c) -> (b -> c) -> Result a b -> c
result f g x = case x of
  Fail y -> f y
  Pass y -> g y

toEither :: Result a b -> Either a b
toEither = result Left Right
