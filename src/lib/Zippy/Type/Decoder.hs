module Zippy.Type.Decoder where

import qualified Control.Applicative as Applicative
import qualified Control.Monad.Fail as Fail
import qualified Data.Functor.Identity as Identity
import qualified Zippy.Type.List as List
import qualified Zippy.Type.Pair as Pair
import qualified Zippy.Type.Result as Result

newtype Decoder s u m a = Decoder (s -> u -> m (Result.Result
  (Pair.Pair (List.List String) String)
  (Pair.Pair s a)))

instance Functor m => Functor (Decoder s u m) where
  fmap f d = Decoder $ \ s1 u -> fmap
    (\ r -> case r of
      Result.Fail p -> Result.Fail p
      Result.Pass (Pair.Pair s2 x) -> Result.Pass . Pair.Pair s2 $f x)
    $ run d s1 u

instance Monad m => Applicative (Decoder s u m) where
  pure x = Decoder $ \ s _ -> pure . Result.Pass $ Pair.Pair s x

  df <*> dx = Decoder $ \ s1 u -> do
    rf <- run df s1 u
    case rf of
      Result.Fail p -> pure $ Result.Fail p
      Result.Pass (Pair.Pair s2 f) -> fmap
        (\ rx -> case rx of
          Result.Fail p -> Result.Fail p
          Result.Pass (Pair.Pair s3 x) -> Result.Pass . Pair.Pair s3 $ f x)
        $ run dx s2 u

instance Monad m => Monad (Decoder s u m) where
  d >>= f = Decoder $ \ s1 u -> do
    r <- run d s1 u
    case r of
      Result.Fail p -> pure $ Result.Fail p
      Result.Pass (Pair.Pair s2 x) -> run (f x) s2 u

instance Monad m => Fail.MonadFail (Decoder s u m) where
  fail e = Decoder $ \ _ _ -> pure . Result.Fail $ Pair.Pair List.Empty e

instance Monad m => Applicative.Alternative (Decoder s u m) where
  empty = Fail.fail "empty"

  dx <|> dy = Decoder $ \ s u -> do
    r <- run dx s u
    case r of
      Result.Fail _ -> run dy s u
      Result.Pass p -> pure $ Result.Pass p

run
  :: Decoder s u m a
  -> s
  -> u
  -> m (Result.Result (Pair.Pair (List.List String) String) (Pair.Pair s a))
run (Decoder f) = f

runSimple :: Decoder s () Identity.Identity a -> s -> Result.Result String a
runSimple d s = case Identity.runIdentity $ run d s () of
  Result.Fail p -> Result.Fail $ format p
  Result.Pass (Pair.Pair _ x) -> Result.Pass x

embed :: Monad m => Decoder s u m a -> s -> u -> Decoder s u m a
embed d s1 u = Decoder $ \ s2 _ -> fmap
  (\ r -> case r of
    Result.Fail p -> Result.Fail p
    Result.Pass (Pair.Pair _ x) -> Result.Pass $ Pair.Pair s2 x)
  $ run d s1 u

format :: Pair.Pair (List.List String) String -> String
format (Pair.Pair ls1 e) = case ls1 of
  List.Empty -> e
  List.Node l ls2 -> l <> ": " <> format (Pair.Pair ls2 e)

get :: Applicative m => Decoder s u m s
get = Decoder $ \ s _ -> pure . Result.Pass $ Pair.Pair s s

label :: Functor m => String -> Decoder s u m a -> Decoder s u m a
label l d = Decoder $ \ s1 u -> fmap
  (\ r -> case r of
    Result.Fail (Pair.Pair ls e) -> Result.Fail $ Pair.Pair (List.Node l ls) e
    Result.Pass p -> Result.Pass p)
  $ run d s1 u
