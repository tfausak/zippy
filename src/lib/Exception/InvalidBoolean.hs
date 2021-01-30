module Exception.InvalidBoolean (InvalidBoolean(..)) where

import qualified Control.Exception as Exception
import qualified Data.Word as Word

newtype InvalidBoolean
  = InvalidBoolean Word.Word8
  deriving (Eq, Show)

instance Exception.Exception InvalidBoolean
