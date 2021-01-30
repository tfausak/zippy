module Exception.UnknownProperty (UnknownProperty(..)) where

import qualified Control.Exception as Exception
import qualified RocketLeague.Str as Str

newtype UnknownProperty
  = UnknownProperty Str.Str
  deriving (Eq, Show)

instance Exception.Exception UnknownProperty
