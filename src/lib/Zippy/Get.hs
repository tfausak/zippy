module Zippy.Get (Get, run, get, put, lift, throw, catch, label, embed) where

import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Exception.Empty as Empty
import qualified Exception.Fail as Fail

newtype Get s m a = Get (s -> m (Either ([String], Exception.SomeException) (s, a)))

instance Functor m => Functor (Get s m) where
  fmap f g = Get $ \ s -> fmap (fmap (fmap f)) $ run g s

instance Monad m => Applicative (Get s m) where
  pure x = Get $ \ s -> pure $ Right (s, x)

  gf <*> gx = Get $ \ s1 -> do
    r <- run gf s1
    case r of
      Left e -> pure $ Left e
      Right (s2, f) -> run (fmap f gx) s2

instance Monad m => Monad (Get s m) where
  g >>= f = Get $ \ s1 -> do
    r <- run g s1
    case r of
      Left e -> pure $ Left e
      Right (s2, x) -> run (f x) s2

instance Monad m => MonadFail (Get s m) where
  fail = throw . Fail.Fail

instance Monad m => Applicative.Alternative (Get s m) where
  empty = throw Empty.Empty

  gx <|> gy = catch gx $ \ e -> let _ = e :: Exception.SomeException in gy

run :: Get s m a -> s -> m (Either ([String], Exception.SomeException) (s, a))
run (Get f) = f

get :: Applicative m => Get s m s
get = Get $ \ s -> pure $ Right (s, s)

put :: Applicative m => s -> Get s m ()
put s = Get $ \ _ -> pure $ Right (s, ())

lift :: Functor m => m a -> Get s m a
lift m = Get $ \ s -> fmap (\ x -> Right (s, x)) m

throw :: (Exception.Exception e, Applicative m) => e -> Get s m a
throw e = Get $ \ _ -> pure $ Left ([], Exception.toException e)

catch :: (Monad m, Exception.Exception e) => Get s m a -> (e -> Get s m a) -> Get s m a
catch g f = Get $ \ s1 -> do
  r <- run g s1
  case r of
    Left (l, e) -> case Exception.fromException e of
      Nothing -> pure $ Left (l, e)
      Just x -> run (f x) s1
    Right (s2, x) -> pure $ Right (s2, x)

label :: Functor m => String -> Get s m a -> Get s m a
label l g = Get $ \ s -> fmap (Bifunctor.first (Bifunctor.first (l :))) $ run g s

embed :: Functor m => Get s m a -> s -> Get t m a
embed g s = Get $ \ t -> fmap (fmap (Bifunctor.first (const t))) $ run g s
