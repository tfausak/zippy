module Exception.InvalidMaxChannels (InvalidMaxChannels(..)) where

import qualified Control.Exception as Exception
import qualified RocketLeague.Property as Property

newtype InvalidMaxChannels
  = InvalidMaxChannels Property.Property
  deriving (Eq, Show)

instance Exception.Exception InvalidMaxChannels
