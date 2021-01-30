module RocketLeague.Boolean (Boolean, fromBool, toBool, fromBytes) where

import qualified Data.ByteString as ByteString
import qualified Zippy.ByteGet as ByteGet
import qualified Exception.InvalidBoolean as InvalidBoolean

newtype Boolean
  = Boolean Bool
  deriving (Eq, Show)

fromBool :: Bool -> Boolean
fromBool = Boolean

toBool :: Boolean -> Bool
toBool (Boolean x) = x

fromBytes :: ByteGet.ByteGet Boolean
fromBytes = ByteGet.label "Boolean" $ do
  x <- ByteGet.take 1
  case ByteString.head x of
    0 -> pure $ fromBool False
    1 -> pure $ fromBool True
    y -> ByteGet.throw $ InvalidBoolean.InvalidBoolean y
