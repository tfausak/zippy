module Exception.OutOfRange (OutOfRange(..)) where

import qualified Control.Exception as Exception
import qualified Data.Typeable as Typeable

data OutOfRange a
  = OutOfRange (a, a) a
  deriving (Eq, Show)

instance (Show a, Typeable.Typeable a) => Exception.Exception (OutOfRange a)
