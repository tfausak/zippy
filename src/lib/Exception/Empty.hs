module Exception.Empty (Empty(..)) where

import qualified Control.Exception as Exception

data Empty
  = Empty
  deriving (Eq, Show)

instance Exception.Exception Empty
