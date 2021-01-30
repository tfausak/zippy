module Exception.MissingMaxChannels (MissingMaxChannels(..)) where

import qualified Control.Exception as Exception

data MissingMaxChannels
  = MissingMaxChannels
  deriving (Eq, Show)

instance Exception.Exception MissingMaxChannels
