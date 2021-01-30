module Exception.InvalidNumFrames (InvalidNumFrames(..)) where

import qualified Control.Exception as Exception
import qualified RocketLeague.Property as Property

newtype InvalidNumFrames
  = InvalidNumFrames Property.Property
  deriving (Eq, Show)

instance Exception.Exception InvalidNumFrames
