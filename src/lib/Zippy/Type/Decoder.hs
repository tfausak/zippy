module Zippy.Type.Decoder where

import qualified Control.Monad.Fail as Fail
import qualified Prelude
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

newtype Decoder s u m a = Decoder (s -> u -> m (Result.Result
  (Pair.Pair (List.List Prelude.String) Prelude.String)
  (Pair.Pair s a)))

instance Prelude.Functor m => Prelude.Functor (Decoder s u m) where
  fmap = map

instance Prelude.Monad m => Prelude.Applicative (Decoder s u m) where
  pure = pure
  (<*>) = (<*>)

instance Prelude.Monad m => Prelude.Monad (Decoder s u m) where
  (>>=) = (>>=)

instance Prelude.Monad m => Fail.MonadFail (Decoder s u m) where
  fail = fail

run
  :: Decoder s u m a
  -> s
  -> u
  -> m (Result.Result (Pair.Pair (List.List Prelude.String) Prelude.String) (Pair.Pair s a))
run (Decoder f) = f

map :: Prelude.Functor m => (a -> b) -> Decoder s u m a -> Decoder s u m b
map f d = Decoder (\ s1 u -> Prelude.fmap
  (\ r -> case r of
    Result.Fail p -> Result.Fail p
    Result.Pass (Pair.Pair s2 x) -> Result.Pass (Pair.Pair s2 (f x)))
  (run d s1 u))

pure :: Prelude.Applicative m => a -> Decoder s u m a
pure x = Decoder (\ s _ -> Prelude.pure (Result.Pass (Pair.Pair s x)))

(<*>) :: Prelude.Monad m => Decoder s u m (a -> b) -> Decoder s u m a -> Decoder s u m b
df <*> dx = Decoder (\ s1 u -> do
  rf <- run df s1 u
  case rf of
    Result.Fail p -> Prelude.pure (Result.Fail p)
    Result.Pass (Pair.Pair s2 f) -> Prelude.fmap
      (\ rx -> case rx of
        Result.Fail p -> Result.Fail p
        Result.Pass (Pair.Pair s3 x) -> Result.Pass (Pair.Pair s3 (f x)))
      (run dx s2 u))

(>>=) :: Prelude.Monad m => Decoder s u m a -> (a -> Decoder s u m b) -> Decoder s u m b
d >>= f = Decoder (\ s1 u -> do
  r <- run d s1 u
  case r of
    Result.Fail p -> Prelude.pure (Result.Fail p)
    Result.Pass (Pair.Pair s2 x) -> run (f x) s2 u)

fail :: Prelude.Applicative m => Prelude.String -> Decoder s u m a
fail e = Decoder (\ _ _ -> Prelude.pure (Result.Fail (Pair.Pair List.Empty e)))

label :: Prelude.Functor m => Prelude.String -> Decoder s u m a -> Decoder s u m a
label l d = Decoder (\ s1 u -> Prelude.fmap
  (\ r -> case r of
    Result.Fail (Pair.Pair ls e) -> Result.Fail (Pair.Pair (List.Node l ls) e)
    Result.Pass p -> Result.Pass p)
  (run d s1 u))
